module VL.Convert where

import VL.Common
import VL.Scalar

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import qualified VL.Expression  as Core
import qualified VL.Parser      as Parser

import Control.Monad.State

type Gensym = State Int

makeGensym :: String -> Gensym String
makeGensym prefix = do i <- get
                       put (succ i)
                       return ("#:" ++ prefix ++ "-" ++ show i)

convert' :: Parser.Expression -> Gensym (Environment Scalar, Core.Expression)
convert' (Parser.Variable x)
    = return (Environment.empty, Core.Variable x)
convert' (Parser.Constant s)
    = do c <- makeGensym "const"
         return (Environment.insert c s Environment.empty, Core.Variable c)
convert' (Parser.Lambda x b)
    = do (env, b') <- convert' b
         return (env, Core.Lambda x b')
convert' (Parser.Application e1 e2)
    = do (env1, e1') <- convert' e1
         (env2, e2') <- convert' e2
         return (env1 `Environment.union` env2, Core.Application e1' e2')
convert' (Parser.Cons e1 e2)
    = do (env1, e1') <- convert' e1
         (env2, e2') <- convert' e2
         return (env1 `Environment.union` env2, Core.Cons e1' e2')

convert :: Parser.Expression -> (Environment Scalar, Core.Expression)
convert = flip evalState 0 . convert'

parseAndConvert :: String -> (Environment Scalar, Core.Expression)
parseAndConvert = convert . Parser.parseExpression