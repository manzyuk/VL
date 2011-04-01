module VL.AbstractAnalysis where

import VL.Common
import VL.Expression
import VL.AbstractValue

import Data.Map (Map)
import qualified Data.Map as Map

type AbstractAnalysis = Map (CoreExpression, AbstractEnvironment) AbstractValue

empty :: AbstractAnalysis
empty = Map.empty

union :: AbstractAnalysis -> AbstractAnalysis -> AbstractAnalysis
union = Map.unionWith unifyValues

unions :: [AbstractAnalysis] -> AbstractAnalysis
unions = foldl union empty

lookup :: CoreExpression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractValue
lookup e env a = fromMaybe AbstractTop (Map.lookup (e, env) a)

insert :: CoreExpression
       -> AbstractEnvironment
       -> AbstractValue
       -> AbstractAnalysis
       -> AbstractAnalysis
insert e env v a = Map.insert (e, env) v a

domain :: AbstractAnalysis -> [(CoreExpression, AbstractEnvironment)]
domain = Map.keys

expand :: CoreExpression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractAnalysis
expand e env a
    | (e, env) `member` a
    = singleton e env AbstractTop
    | otherwise
    = empty

member :: (CoreExpression, AbstractEnvironment) -> AbstractAnalysis -> Bool
member = Map.member

singleton :: CoreExpression
          -> AbstractEnvironment
          -> AbstractValue
          -> AbstractAnalysis
singleton e env v = Map.singleton (e, env) v
