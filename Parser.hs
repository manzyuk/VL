{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module VL.Parser where

import VL.Common
import VL.Coproduct

import VL.Scalar (Scalar, ScalarEnvironment)
import qualified VL.Scalar as Scalar

import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.Token (Token, scan)
import qualified VL.Token as Token

import VL.Pretty (pp, render)

import Text.Parsec.Prim       hiding (many, (<|>), State, token, parse)
import Text.Parsec.String     hiding (Parser)
import Text.Parsec.Combinator (between)

import Control.Applicative
import Control.Monad.State
import Control.Arrow (first, second, (***))

nil, true, false :: Name
nil   = "#:nil"
true  = "#:true"
false = "#:false"

-- A custom parser type that carries around additional state used for
-- constant conversion.  The state consists of a scalar environment
-- and an integer count that is used to generate unique variable
-- names.
type Parser = ParsecT [Token] () (State (ScalarEnvironment, Int))

-- Accepts a token @t@ with the result @x@ when @test t@ is @Just x@.
-- Like @token@ from Text.Parsec.Prim, but uses the default token
-- pretty-printing function and ignores the position info.
token :: (Token -> Maybe a) -> Parser a
token test = tokenPrim show (\pos _ _ -> pos) test

-- Accepts a token @t@ with the result @k@ when @t@ is the identifier
-- @k@.
symbol :: String -> Parser String
symbol k = token maybeSymbol
    where
      maybeSymbol (Token.Identifier n)
          | k == n    = Just k
          | otherwise = Nothing
      maybeSymbol _   = Nothing

-- Accepts the token @t@ with the result @t@.
literate :: Token -> Parser Token
literate t = token maybeLiterate
    where
      maybeLiterate x
          | x == t    = Just x
          | otherwise = Nothing

lparen, rparen :: Parser Token
lparen = literate Token.LParen
rparen = literate Token.RParen

-- Accepts a token @t@ with the result @n@ when @t@ is the identifier
-- @n@ that is not a reserved keyword.
identifier :: Parser Name
identifier = token maybeIdentifier
    where
      maybeIdentifier (Token.Identifier x)
          | x `notElem` keywords = Just x
          | otherwise            = Nothing
      maybeIdentifier _          = Nothing

-- The list of reserved keywords.
keywords :: [String]
keywords = [ "lambda"
           , "cons"
           , "list"
           , "cons*"
           , "if"
           , "or"
           , "and"
           , "cond"
           , "let"
           , "letrec"
           ]

parseVariable :: Parser SurfaceExpression
parseVariable = mkVariable <$> identifier

parseConstant :: Parser SurfaceExpression
parseConstant = do s <- try parseEmptyList <|> token maybeConstant
                   (env, i) <- get
                   -- (), #t, #f are always converted to the same
                   -- global names "#:nil", "#:true", "#:false".
                   let x = case s of
                             Scalar.Nil           -> nil
                             Scalar.Boolean True  -> true
                             Scalar.Boolean False -> false
                             Scalar.Real _        -> "#:real-" ++ show i
                   put (Environment.update x s env, succ i)
                   return (mkVariable x)
    where
      maybeConstant (Token.Boolean b) = Just (Scalar.Boolean b)
      maybeConstant (Token.Real    r) = Just (Scalar.Real    r)
      maybeConstant _                 = Nothing

parseEmptyList :: Parser Scalar
parseEmptyList = Scalar.Nil <$ (lparen >> rparen)

-- Helper combinators and aliases for better readability.
special :: String -> Parser a -> Parser a
special name body = symbol name *> body

parens :: Parser a -> Parser a
parens = between lparen rparen

listOf :: Parser a -> Parser [a]
listOf p = parens (many p)

args :: Parser [Name]
args = listOf identifier

body :: Parser SurfaceExpression
body = expression

-- Expression parsers.
parseLambda
    = special "lambda"  $ liftA2 mkLambdaManyArgs args body
parseCons
    = special "cons"    $ liftA2 mkCons expression expression
parseList
    = special "list"    $ liftA  mkList (many expression)
parseConsStar
    = special "cons*"   $ liftA  mkConsStar (many expression)
parseIf
    = special "if"      $ liftA3 mkIf predicate consequent alternate
    where
      predicate  = expression
      consequent = expression
      alternate  = expression
parseOr
    = special "or"      $ liftA  mkOr (many expression)
parseAnd
    = special "and"     $ liftA  mkAnd (many expression)
parseCond
    = special "cond"    $ liftA  mkCond clauses
    where
      clauses  = many clause
      clause   = parens $ liftA2 (,) test expression
      test     = expression
parseLet
    = special "let"     $ liftA2 mkLet bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA2 (,) identifier expression
parseLetrec
    = special "letrec"  $ liftA2 mkLetrecManyArgs bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA3 (,,) identifier args body

parseApplication = liftA2 mkApplicationManyArgs expression (many expression)

-- expandList, expandConsStar :: [SurfaceExpression] -> SurfaceExpression
-- expandList     = foldr  Cons (Variable nil)
-- expandConsStar = foldr' Cons (Variable nil)

-- foldr' :: (b -> b -> b) -> b -> [b] -> b
-- foldr' step x0 []     = x0
-- foldr' step x0 [x1]   = x1
-- foldr' step x0 (x:xs) = step x (foldr' step x0 xs)

expression :: Parser SurfaceExpression
expression = atom <|> list
    where
      atom = parseVariable <|> parseConstant
      list = parens $
                 try parseLambda
             <|> try parseCons
             <|> try parseList
             <|> try parseConsStar
             <|> try parseIf
             <|> try parseOr
             <|> try parseAnd
             <|> try parseCond
             <|> try parseLet
             <|> try parseLetrec
             <|> parseApplication

parseAndConvertConstants :: String -> (SurfaceExpression, ScalarEnvironment)
parseAndConvertConstants
    = ((either (\_ -> error "parse error") id) *** fst)
    . flip runState (initialEnvironment, 0)
    . runParserT expression () ""
    . scan
    where
      initialEnvironment
          = Environment.fromList
            [ (nil,   Scalar.Nil          )
            , (true,  Scalar.Boolean True )
            , (false, Scalar.Boolean False)
            ]

-- Code transformations

-- Elimination of derived conditionals (`or', `and', `cond')
type Stage1  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: If
            :+: Let
            :+: LetrecManyArgs

elimConditionals :: Expr Surface -> Expr Stage1
elimConditionals = foldExpr elimConditionalsAlg

class Functor f => ElimConditionals f where
    elimConditionalsAlg :: f(Expr Stage1) -> Expr Stage1

instance ElimConditionals Variable where
    elimConditionalsAlg (Variable x) = mkVariable x

instance ElimConditionals LambdaManyArgs where
    elimConditionalsAlg (LambdaManyArgs args body)
        = mkLambdaManyArgs args body

instance ElimConditionals ApplicationManyArgs where
    elimConditionalsAlg (ApplicationManyArgs operator operands)
        = mkApplicationManyArgs operator operands

instance ElimConditionals Cons where
    elimConditionalsAlg (Cons x y) = mkCons x y

instance ElimConditionals List where
    elimConditionalsAlg (List xs) = mkList xs

instance ElimConditionals ConsStar where
    elimConditionalsAlg (ConsStar xs) = mkConsStar xs

instance ElimConditionals If where
    elimConditionalsAlg (If predicate consequent alternate)
        = mkIf predicate consequent alternate

instance ElimConditionals Or where
    elimConditionalsAlg (Or xs) = foldr wrap (mkVariable false) xs
        where
          wrap x y = mkIf x (mkVariable true) y

instance ElimConditionals And where
    elimConditionalsAlg (And xs) = foldr wrap (mkVariable true) xs
        where
          wrap x y = mkIf x y (mkVariable false)

instance ElimConditionals Cond where
    elimConditionalsAlg (Cond clauses) = foldr wrap (mkVariable nil) clauses
        where
          wrap (t, e) e' = mkIf t e e'

instance ElimConditionals Let where
    elimConditionalsAlg (Let bindings body) = mkLet bindings body

instance ElimConditionals LetrecManyArgs where
    elimConditionalsAlg (LetrecManyArgs bindings body)
        = mkLetrecManyArgs bindings body

instance (Functor f, ElimConditionals f, Functor g, ElimConditionals g) =>
    ElimConditionals (f :+: g) where
        elimConditionalsAlg (Inl x) = elimConditionalsAlg x
        elimConditionalsAlg (Inr x) = elimConditionalsAlg x

-- Elimination of `if'
type Stage2  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: Let
            :+: LetrecManyArgs

elimIf :: Expr Stage1 -> Expr Stage2
elimIf = foldExpr elimIfAlg

class Functor f => ElimIf f where
    elimIfAlg :: f (Expr Stage2) -> Expr Stage2

instance ElimIf Variable where
    elimIfAlg (Variable x) = mkVariable x

instance ElimIf LambdaManyArgs where
    elimIfAlg (LambdaManyArgs args body) = mkLambdaManyArgs args body

instance ElimIf ApplicationManyArgs where
    elimIfAlg (ApplicationManyArgs operator operands)
        = mkApplicationManyArgs operator operands

instance ElimIf Cons where
    elimIfAlg (Cons x y) = mkCons x y

instance ElimIf List where
    elimIfAlg (List xs) = mkList xs

instance ElimIf ConsStar where
    elimIfAlg (ConsStar xs) = mkConsStar xs

instance ElimIf If where
    elimIfAlg (If predicate consequent alternate)
        = mkApplicationManyArgs (mkVariable "#:if-procedure")
                                [(thunk consequent), (thunk alternate)]
        where
          thunk e = mkLambdaManyArgs [] e

instance ElimIf Let where
    elimIfAlg (Let bindings body) = mkLet bindings body

instance ElimIf LetrecManyArgs where
    elimIfAlg (LetrecManyArgs bindings body)
        = mkLetrecManyArgs bindings body

instance (Functor f, ElimIf f, Functor g, ElimIf g) => ElimIf (f :+: g) where
    elimIfAlg (Inl x) = elimIfAlg x
    elimIfAlg (Inr x) = elimIfAlg x

-- Elimination of `let'
type Stage3  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: LetrecManyArgs

elimLet :: Expr Stage2 -> Expr Stage3
elimLet = foldExpr elimLetAlg

class Functor f => ElimLet f where
    elimLetAlg :: f (Expr Stage3) -> Expr Stage3

instance ElimLet Variable where
    elimLetAlg (Variable x) = mkVariable x

instance ElimLet LambdaManyArgs where
    elimLetAlg (LambdaManyArgs args body) = mkLambdaManyArgs args body

instance ElimLet ApplicationManyArgs where
    elimLetAlg (ApplicationManyArgs operator operands)
        = mkApplicationManyArgs operator operands

instance ElimLet Cons where
    elimLetAlg (Cons x y) = mkCons x y

instance ElimLet List where
    elimLetAlg (List xs) = mkList xs

instance ElimLet ConsStar where
    elimLetAlg (ConsStar xs) = mkConsStar xs

instance ElimLet Let where
    elimLetAlg (Let bindings body)
        = mkApplicationManyArgs (mkLambdaManyArgs args body) vals
        where
          (args, vals) = unzip bindings

instance ElimLet LetrecManyArgs where
    elimLetAlg (LetrecManyArgs bindings body)
        = mkLetrecManyArgs bindings body

instance (Functor f, ElimLet f, Functor g, ElimLet g) => ElimLet (f :+: g) where
    elimLetAlg (Inl x) = elimLetAlg x
    elimLetAlg (Inr x) = elimLetAlg x

-- Elimination of many arguments (in lambdas, applications, and letrec bindings)
type Stage4  =  Variable
            :+: LambdaOneArg
            :+: ApplicationOneArg
            :+: Cons
            :+: List
            :+: ConsStar
            :+: LetrecOneArg

elimManyArgs :: Expr Stage3 -> Expr Stage4
elimManyArgs = foldExpr elimManyArgsAlg

class Functor f => ElimManyArgs f where
    elimManyArgsAlg :: f (Expr Stage4) -> Expr Stage4

instance ElimManyArgs Variable where
    elimManyArgsAlg (Variable x) = mkVariable x

instance ElimManyArgs LambdaManyArgs where
    elimManyArgsAlg (LambdaManyArgs args body)
        = mkLambdaOneArg arg body'
        where
          (arg, body') = nestLambdas args body

-- Transform a lambda that takes possibly many arguments into a
-- combination of nested lambdas that take only one argument,
-- introducing suitable argument destructuring.  In particular:
--
-- (lambda () e)
-- ~> (lambda (#:ignored) e)
--
-- and
--
-- (lambda (x1 x2) e)
-- ~> (lambda (#:args)
--      ((lambda (x1)
--         ((lambda (x2)
--            e)
--          (cdr #:args)))
--       (car #:args)))
nestLambdas :: [Name] -> Expr Stage4 -> (Name, Expr Stage4)
nestLambdas []    body = ("#:ignored", body)
nestLambdas [arg] body = (arg, body)
nestLambdas args  body = ("#:args", body'')
    where
      p             = mkVariable "#:args"
      n             = length args
      argn          = last args
      body'         = with argn (cdnr (n-1) p) body
      body''        = foldr wrap body' (zip args [0..n-2])
      wrap (x, k) e = with x (cadnr k p) e
      with x v e    = mkApplicationOneArg (mkLambdaOneArg x e) v

cdnr, cadnr :: (ApplicationOneArg :<: f, Variable :<: f)
            => Int -> Expr f -> Expr f
cdnr n = compose (replicate n cdr)
    where
      compose = foldr (.) id
      cdr = mkApplicationOneArg (mkVariable "cdr")

cadnr n = car . cdnr n
    where
      car = mkApplicationOneArg (mkVariable "car")

instance ElimManyArgs ApplicationManyArgs where
    elimManyArgsAlg (ApplicationManyArgs operator operands)
        = mkApplicationOneArg operator (mkConsStar operands)

instance ElimManyArgs Cons where
    elimManyArgsAlg (Cons x y) = mkCons x y

instance ElimManyArgs List where
    elimManyArgsAlg (List xs) = mkList xs

instance ElimManyArgs ConsStar where
    elimManyArgsAlg (ConsStar xs) = mkConsStar xs

instance ElimManyArgs LetrecManyArgs where
    elimManyArgsAlg (LetrecManyArgs bindings body)
        = mkLetrecOneArg bindings' body
        where
          bindings' = [ (name, arg, e')
                      | (name, args, e) <- bindings
                      , let (arg, e') = nestLambdas args e
                      ]

instance (Functor f, ElimManyArgs f, Functor g, ElimManyArgs g) =>
    ElimManyArgs (f :+: g) where
        elimManyArgsAlg (Inl x) = elimManyArgsAlg x
        elimManyArgsAlg (Inr x) = elimManyArgsAlg x

-- parse :: String -> (CoreExpression, ScalarEnvironment)
-- parse = first transform . parseAndConvertConstants
