module VL.Parser where

import VL.Common
import VL.Scalar

import VL.Token (Token, scan)
import qualified VL.Token as Token

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), Parser)
import Control.Applicative

data Expression
    = Variable Name
    | Constant Scalar
    | Lambda Name Expression
    | Application Expression Expression
    | Cons Expression Expression
      deriving (Show)

type Parser a = GenParser Token () a

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
identifier :: Parser String
identifier = extract getIdentifier <?> "identifier"
    where
      getIdentifier (Token.Identifier x)
          | x `notElem` keywords = Just x
          | otherwise            = Nothing
      getIdentifier _            = Nothing

variable :: Parser Expression
variable = Variable <$> identifier

keywords :: [String]
keywords = ["lambda", "cons"]

-- @constant@ is a parser that succeeds if the next token is a
-- constant (i.e., nil, #t, #f, or a real) and returns it
-- suitably wrapped, or fails otherwise.
constant :: Parser Expression
constant = Constant <$> (try emptyList <|> extract getConstant) <?> "constant"
    where
      getConstant (Token.Boolean b) = Just (Boolean b)
      getConstant (Token.Real    r) = Just (Real    r)
      getConstant _                 = Nothing

emptyList :: Parser Scalar
emptyList = Nil <$ (literate Token.Quote >> literate Token.LParen >> literate Token.RParen)

parens :: Parser a -> Parser a
parens = between (literate Token.LParen) (literate Token.RParen)

lambda, application, cons :: Parser Expression
lambda      = liftA2 Lambda      (keyword "lambda" *> parens identifier) expression
application = liftA2 Application expression                              expression
cons        = liftA2 Cons        (keyword "cons"   *> expression)        expression

expression :: Parser Expression
expression = atom <|> list
    where
      atom = variable <|> constant
      list = parens (try lambda <|> try cons <|> application)

parseExpression :: String -> Expression
parseExpression = either (\_ -> error "parse error") id
                . parse (expression <* eof) ""
                . scan
