module VL.Parser where

import VL.Common

import VL.Scalar (Scalar, ScalarEnvironment)
import qualified VL.Scalar as Scalar

import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.Token (Token, scan)
import qualified VL.Token as Token

import VL.Pretty (pp, render)

import Text.Parsec.Prim       hiding (many, (<|>), State, parse)
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

pExtract :: (Token -> Maybe a) -> Parser a
pExtract selector = tokenPrim show (\pos _ _ -> pos) selector

pKeyword :: String -> Parser String
pKeyword k = pExtract maybeKeyword
    where
      maybeKeyword (Token.Identifier n)
          | k == n    = Just k
          | otherwise = Nothing
      maybeKeyword _  = Nothing

pLiterate :: Token -> Parser Token
pLiterate t = pExtract maybeLiterate
    where
      maybeLiterate x
          | x == t    = Just x
          | otherwise = Nothing

pIdentifier :: Parser Name
pIdentifier = pExtract maybeIdentifier
    where
      maybeIdentifier (Token.Identifier x)
          | x `notElem` keywords = Just x
          | otherwise            = Nothing
      maybeIdentifier _          = Nothing

pVariable :: Parser SurfaceExpression
pVariable = mkVariable <$> pIdentifier

keywords :: [String]
keywords = ["lambda", "cons", "list", "cons*", "if", "or", "and", "cond", "let", "letrec"]

-- @constant@ is a parser that consumes the next token and fails if it
-- is not a constant; otherwise it generates a variable name and binds
-- it to the constant in the environment.  The empty list and booleans
-- are always bound to the same names.
pConstant :: Parser SurfaceExpression
pConstant = do s <- try pEmptyList <|> pExtract maybeConstant
               (env, i) <- get
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

pEmptyList :: Parser Scalar
pEmptyList = Scalar.Nil <$ (pLiterate Token.LParen >> pLiterate Token.RParen)

parens :: Parser a -> Parser a
parens = between (pLiterate Token.LParen) (pLiterate Token.RParen)

special :: String -> Parser a -> Parser a
special name body = pKeyword name *> body

listOf :: Parser a -> Parser [a]
listOf p = parens (many p)

pLambdaManyArgs = special "lambda"
                $ liftA2 mkLambdaManyArgs (listOf pIdentifier) pExpression
pCons           = special "cons"
                $ liftA2 mkCons pExpression pExpression
pList           = special "list"
                $ liftA mkList (many pExpression)
pConsStar       = special "cons*"
                $ liftA mkConsStar (many pExpression)
pIf             = special "if"
                $ liftA3 mkIf pExpression pExpression pExpression
pOr             = special "or"
                $ liftA mkOr (many pExpression)
pAnd            = special "and"
                $ liftA mkAnd (many pExpression)
pCond           = special "cond"
                $ liftA mkCond (many pClause)
    where
      pClause = parens $ liftA2 (,) pExpression pExpression
pLet            = special "let"
                $ liftA2 mkLet (listOf pBinding) pExpression
    where
      pBinding = parens $ liftA2 (,) pIdentifier pExpression
pLetrecManyArgs = special "letrec"
                $ liftA2 mkLetrecManyArgs (listOf pBinding) pExpression
    where
      pBinding = parens $ liftA3 (,,) pIdentifier (listOf pIdentifier) pExpression

pApplicationManyArgs = liftA2 mkApplicationManyArgs pExpression (many pExpression)

-- expandList, expandConsStar :: [SurfaceExpression] -> SurfaceExpression
-- expandList     = foldr  Cons (Variable nil)
-- expandConsStar = foldr' Cons (Variable nil)

-- foldr' :: (b -> b -> b) -> b -> [b] -> b
-- foldr' step x0 []     = x0
-- foldr' step x0 [x1]   = x1
-- foldr' step x0 (x:xs) = step x (foldr' step x0 xs)

pExpression :: Parser SurfaceExpression
pExpression = pAtom <|> pForm
    where
      pAtom = pVariable <|> pConstant
      pForm = parens $
                 try pLambdaManyArgs
             <|> try pCons
             <|> try pList
             <|> try pConsStar
             <|> try pIf
             <|> try pOr
             <|> try pAnd
             <|> try pCond
             <|> try pLet
             <|> try pLetrecManyArgs
             <|> pApplicationManyArgs

parseAndConvertConstants :: String -> (SurfaceExpression, ScalarEnvironment)
parseAndConvertConstants
    = ((either (\_ -> error "parse error") id) *** fst)
    . flip runState (initialEnvironment, 0)
    . runParserT pExpression () ""
    . scan
    where
      initialEnvironment
          = Environment.fromList
            [ (nil,   Scalar.Nil          )
            , (true,  Scalar.Boolean True )
            , (false, Scalar.Boolean False)
            ]

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
