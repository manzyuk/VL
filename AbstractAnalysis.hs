module VL.AbstractAnalysis where

import VL.Common
import VL.Expression
import VL.AbstractValue

import Data.Map (Map)
import qualified Data.Map as Map

type AbstractAnalysis = Map (Expression, AbstractEnvironment) AbstractValue

empty :: AbstractAnalysis
empty = Map.empty

union :: AbstractAnalysis -> AbstractAnalysis -> AbstractAnalysis
union = Map.unionWith unifyValues

unions :: [AbstractAnalysis] -> AbstractAnalysis
unions = foldl union empty

lookup :: Expression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractValue
lookup e env a = fromMaybe AbstractTop (Map.lookup (e, env) a)

insert :: Expression
       -> AbstractEnvironment
       -> AbstractValue
       -> AbstractAnalysis
       -> AbstractAnalysis
insert e env v a = Map.insert (e, env) v a

domain :: AbstractAnalysis -> [(Expression, AbstractEnvironment)]
domain = Map.keys

expand :: Expression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractAnalysis
expand e env a
    | (e, env) `member` a
    = singleton e env AbstractTop
    | otherwise
    = empty

member :: (Expression, AbstractEnvironment) -> AbstractAnalysis -> Bool
member = Map.member

singleton :: Expression
          -> AbstractEnvironment
          -> AbstractValue
          -> AbstractAnalysis
singleton e env v = Map.singleton (e, env) v
