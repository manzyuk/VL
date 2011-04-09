{-# LANGUAGE TypeOperators #-}
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

-- Elimination of derived conditionals (or, and, cond)

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
    elimConditionalsAlg (Cons x1 x2) = mkCons x1 x2

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

instance (Functor f, ElimConditionals f, Functor g, ElimConditionals g)
    => ElimConditionals (f :+: g) where
    elimConditionalsAlg (Inl x) = elimConditionalsAlg x
    elimConditionalsAlg (Inr x) = elimConditionalsAlg x

-- cdnr :: Int -> Expression binder -> Expression binder
-- cdnr n = compose (replicate n cdr)
--     where
--       compose = foldr (.) id
--       cdr = Application (Variable "cdr")

-- cadnr :: Int -> Expression binder -> Expression binder
-- cadnr n = car . cdnr n
--     where
--       car = Application (Variable "car")

-- -- Transform every lambda that takes possibly many arguments into a
-- -- combination of lambdas that take only one argument, introducing
-- -- suitable argument destructuring.  In particular:
-- --
-- -- (lambda () e)
-- -- ~> (lambda (#:ignored) e)
-- --
-- -- and
-- --
-- -- (lambda (x1 x2) e)
-- -- ~> (lambda (#:args)
-- --      ((lambda (x1)
-- --         ((lambda (x2)
-- --            e)
-- --          (cdr #:args)))
-- --       (car #:args)))
-- transform :: SurfaceExpression -> CoreExpression
-- transform (Lambda []  b) = Lambda "#:ignored" (transform b)
-- transform (Lambda [x] b) = Lambda x (transform b)
-- transform (Lambda xs  b) = Lambda "#:args" b''
--     where
--       p   = Variable "#:args"
--       n   = length xs
--       xn  = last xs
--       b'  = Application (Lambda xn (transform b)) (cdnr (n-1) p)
--       b'' = foldr wrap b' (zip xs [0..n-2])
--       wrap (x, k) e = Application (Lambda x e) (cadnr k p)
-- transform (Variable x) = Variable x
-- transform (Application e1 e2)
--     = Application (transform e1) (transform e2)
-- transform (Cons e1 e2)
--     = Cons (transform e1) (transform e2)
-- transform (Letrec ls e)
--     = Letrec ls' e'
--     where
--       e'  = transform e
--       ls' = [ (name, x, b')
--             | (name, xs, b) <- ls
--             , let Lambda x b' = transform (Lambda xs b)
--             ]

-- parse :: String -> (CoreExpression, ScalarEnvironment)
-- parse = first transform . parseAndConvertConstants
