module VL.Parser where

import VL.Common

import VL.Scalar (Scalar, ScalarEnvironment)
import qualified VL.Scalar as Scalar

import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.Token (Token, scan)
import qualified VL.Token as Token

import Text.Parsec.Prim       hiding (many, (<|>), State, parse)
import Text.Parsec.String     hiding (Parser)
import Text.Parsec.Combinator

import Control.Applicative
import Control.Monad.State
import Control.Arrow (first, (***))

nil, true, false :: Name
nil   = "#:nil"
true  = "#:true"
false = "#:false"

type Parser = ParsecT [Token] () (State (ScalarEnvironment, Int))

-- @extract selector@ is a parser that consumes one token @t@ and
-- fails if @selector t@ is @Nothing@ or returns @x@ such that
-- @selector t == Just x@.
extract :: (Token -> Maybe a) -> Parser a
extract selector = tokenPrim show (\pos _ _ -> pos) selector

-- @keyword k@ is a parser that succeeds if the next token is an
-- identifier @k@ and returns @k@, or fails otherwise.
keyword :: String -> Parser String
keyword k = extract getKeyword
    where
      getKeyword (Token.Identifier n)
          | k == n    = Just k
          | otherwise = Nothing
      getKeyword _    = Nothing

-- @literate t@ is a parser that succeeds if the next token is equal
-- to the supplied token @t@ and returns @t@, or fails otherwise.
literate :: Token -> Parser Token
literate t = extract getLiterate
    where
      getLiterate x
          | x == t    = Just x
          | otherwise = Nothing

-- @identifier@ is a parser that succeeds if the next token is an
-- identifier that is not a keyword and returns the name of that
-- identifier, or fails otherwise.
identifier :: Parser Name
identifier = extract getIdentifier
    where
      getIdentifier (Token.Identifier x)
          | x `notElem` keywords = Just x
          | otherwise            = Nothing
      getIdentifier _            = Nothing

variable :: Parser SurfaceExpression
variable = Variable <$> identifier

keywords :: [String]
keywords = ["lambda", "cons", "list", "cons*"]

constant :: Parser SurfaceExpression
constant = do s <- try emptyList <|> extract getConstant
              (env, i) <- get
              let x = case s of
                        Scalar.Nil           -> nil
                        Scalar.Boolean True  -> true
                        Scalar.Boolean False -> false
                        Scalar.Real _        -> "#:real-" ++ show i
              put (Environment.update x s env, succ i)
              return (Variable x)
    where
      getConstant (Token.Boolean b) = Just (Scalar.Boolean b)
      getConstant (Token.Real    r) = Just (Scalar.Real    r)
      getConstant _                 = Nothing

-- -- @constant@ is a parser that succeeds if the next token is a
-- -- constant (i.e., nil, #t, #f, or a real) and returns it
-- -- suitably wrapped, or fails otherwise.
-- constant :: Parser (Expression binder)
-- constant = Constant <$> (try emptyList <|> extract getConstant)
--     where
--       getConstant (Token.Boolean b) = Just (Boolean b)
--       getConstant (Token.Real    r) = Just (Real    r)
--       getConstant _                 = Nothing

emptyList :: Parser Scalar
emptyList = Scalar.Nil <$ (literate Token.LParen >> literate Token.RParen)

parens :: Parser a -> Parser a
parens = between (literate Token.LParen) (literate Token.RParen)

special :: String -> Parser a -> Parser a
special name body = keyword name *> body

lambda, cons, list, consStar, application :: Parser SurfaceExpression
lambda      = special "lambda" $ liftA2 Lambda (parens (many identifier)) expression
cons        = special "cons"   $ liftA2 Cons   expression                 expression
list        = special "list"   $ expandList     <$> (many expression)
consStar    = special "cons*"  $ expandConsStar <$> (many expression)
application = liftA2 Application expression $ expandConsStar <$> (many expression)

expandList, expandConsStar :: [SurfaceExpression] -> SurfaceExpression
expandList     = foldr  Cons (Variable nil)
expandConsStar = foldr' Cons (Variable nil)

foldr' :: (b -> b -> b) -> b -> [b] -> b
foldr' step x0 []     = x0
foldr' step x0 [x1]   = x1
foldr' step x0 (x:xs) = step x (foldr' step x0 xs)

expression :: Parser SurfaceExpression
expression = atom <|> form
    where
      atom = variable <|> constant
      form = parens $
                 try lambda
             <|> try cons
             <|> try list
             <|> try consStar
             <|> application

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

cdnr :: Int -> Expression binder -> Expression binder
cdnr n = compose (replicate n cdr)
    where
      compose = foldr (.) id
      cdr = Application (Variable "cdr")

cadnr :: Int -> Expression binder -> Expression binder
cadnr n = car . cdnr n
    where
      car = Application (Variable "car")

transform :: SurfaceExpression -> CoreExpression
transform (Lambda []  b) = Lambda "#:ignored" (transform b)
transform (Lambda [x] b) = Lambda x (transform b)
transform (Lambda xs  b) = Lambda "#:args" b'
    where
      p  = Variable "#:args"
      b' = foldr wrap (transform b) (zip xs [0..])
      wrap (x, n) e = Application (Lambda x e) (cadnr n p)
transform (Variable x) = Variable x
transform (Application e1 e2)
    = Application (transform e1) (transform e2)
transform (Cons e1 e2)
    = Cons (transform e1) (transform e2)

parse :: String -> (CoreExpression, ScalarEnvironment)
parse = first transform . parseAndConvertConstants
