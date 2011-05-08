module VL.Language.Parser (parse) where

import VL.Language.Common

import VL.Language.Scalar (Scalar, ScalarEnvironment)
import qualified VL.Language.Scalar as Scalar

import VL.Language.Syntax
import VL.Language.Pretty (pprint)

import qualified VL.Language.Environment as Environment

import VL.Language.Token (Token)
import qualified VL.Language.Token as Token

import Text.Parsec.Prim       hiding (many, (<|>), State, token, parse)
import Text.Parsec.String     ()
import Text.Parsec.Combinator (between)

import Control.Applicative
import Control.Monad.State
import Control.Arrow

-- A custom parser type that carries around additional state used for
-- constant conversion.  The state consists of a scalar environment
-- and an integer count that is used to generate unique variable
-- names.
type Parser = ParsecT [Token] () (State (ScalarEnvironment, Int))

-- Accepts a token 't' with the result 'x' when 'test t' is 'Just x'.
-- Like 'token' from Text.Parsec.Prim, but uses the default token
-- pretty-printing function and ignores the position info.
token :: (Token -> Maybe a) -> Parser a
token test = tokenPrim show (\pos _ _ -> pos) test

-- Accepts the token 't' with the result 't'.
literal :: Token -> Parser Token
literal t = token maybeLiteral
    where
      maybeLiteral x
	  | x == t    = Just x
	  | otherwise = Nothing

lparen, rparen :: Parser Token
lparen = literal Token.LParen
rparen = literal Token.RParen

-- Accepts a token 't' with the result 'n' when 't' is the identifier
-- 'n' that is not a reserved keyword.
identifier :: Parser Name
identifier = token maybeIdentifier
    where
      maybeIdentifier (Token.Identifier x)
	  | x `notElem` keywords = Just x
	  | otherwise            = Nothing
      maybeIdentifier _          = Nothing

-- Accepts a token 't' with the result 'n' when 't' is the identifier
-- 'n' that is a reserved keyword.
keyword :: Parser Name
keyword = token maybeKeyword
    where
      maybeKeyword (Token.Identifier x)
	  | x `elem` keywords = Just x
	  | otherwise         = Nothing
      maybeKeyword _          = Nothing

parseVariable :: Parser SurfaceSyntax
parseVariable = mkVariable <$> identifier

parseConstant :: Parser SurfaceSyntax
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
parens :: Parser a -> Parser a
parens = between lparen rparen

listOf :: Parser a -> Parser [a]
listOf p = parens (many p)

formals :: Parser [Name]
formals = listOf identifier

body :: Parser SurfaceSyntax
body = expression

-- The association list of special form names to the their parsers.
specialForms :: [(String, Parser SurfaceSyntax)]
specialForms = [ ("lambda", parseLambda  )
	       , ("cons"  , parseCons    )
	       , ("list"  , parseList    )
	       , ("cons*" , parseConsStar)
	       , ("if"    , parseIf      )
	       , ("or"    , parseOr      )
	       , ("and"   , parseAnd     )
	       , ("not"   , parseNot     )
	       , ("cond"  , parseCond    )
	       , ("let"   , parseLet     )
	       , ("letrec", parseLetrec  )
	       ]

-- The list of reserved keywords.
keywords :: [String]
keywords = map fst specialForms

-- Special form parsers.
parseLambda, parseCons, parseList, parseConsStar :: Parser SurfaceSyntax
parseIf, parseOr, parseAnd, parseNot, parseCond  :: Parser SurfaceSyntax
parseLet, parseLetrec                            :: Parser SurfaceSyntax
parseLambda
    = liftA2 mkLambdaManyArgs formals body
parseCons
    = liftA2 mkCons expression expression
parseList
    = liftA  mkList (many expression)
parseConsStar
    = liftA  mkConsStar (many expression)
parseIf
    = liftA3 mkIf predicate consequent alternate
    where
      predicate  = expression
      consequent = expression
      alternate  = expression
parseOr
    = liftA  mkOr (many expression)
parseAnd
    = liftA  mkAnd (many expression)
parseNot
    = liftA  mkNot expression
parseCond
    = liftA  mkCond clauses
    where
      clauses  = many clause
      clause   = parens $ liftA2 (,) test expression
      test     = expression
parseLet
    = liftA2 mkLet bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA2 (,) identifier expression
parseLetrec
    = liftA2 mkLetrecManyArgs bindings body
    where
      bindings = listOf binding
      binding  = parens $ liftA3 (,,) identifier formals body

parseSpecialForm, parseApplication :: Parser SurfaceSyntax
parseSpecialForm = fromMaybe empty . flip lookup specialForms =<< keyword
parseApplication = liftA2 mkApplicationManyArgs expression (many expression)

expression :: Parser SurfaceSyntax
expression = atom <|> list
    where
      atom = parseVariable <|> parseConstant
      list = parens $ try parseSpecialForm
		      <|> parseApplication

parse :: String -> (SurfaceSyntax, ScalarEnvironment)
parse = ((either (\_ -> error "parse error") id) *** fst)
      . flip runState (initialEnvironment, 0)
      . runParserT expression () ""
      . Token.scan
    where
      initialEnvironment
	  = Environment.fromList
	    [ (nil,   Scalar.Nil          )
	    , (true,  Scalar.Boolean True )
	    , (false, Scalar.Boolean False)
	    ]
