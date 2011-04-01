module VL.Convert where

import VL.Common
import VL.Scalar

import VL.Environment (Environment)
import qualified VL.Environment as Environment

-- I dont' know what's worse: to have almost identical definitions of
-- data types in separate modules and `import qualified' them or
-- prepend all type and constructor names with some prefix.
import qualified VL.Expression  as Core
import qualified VL.Parser      as Parser

import Control.Monad.State

type Gensym = State Int

mkGensym :: String -> Gensym String
mkGensym prefix = do i <- get
                     put (succ i)
                     return ("#:" ++ prefix ++ "-" ++ show i)

convert' :: Parser.Expression Name -> Gensym (Environment Scalar, Core.Expression)
convert' (Parser.Variable x)
    = return (Environment.empty, Core.Variable x)
convert' (Parser.Constant s)
    = do c <- mkGensym "const"
         return (Environment.singleton c s, Core.Variable c)
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

convert :: Parser.Expression Name -> (Environment Scalar, Core.Expression)
convert = flip evalState 0 . convert'

parseAndConvert :: String -> (Environment Scalar, Core.Expression)
parseAndConvert = convert . Parser.parseExpression