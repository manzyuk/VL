module VL.AbstractAnalysis
    ( AbstractAnalysis
    , empty
    , union
    , unions
    , lookup
    , insert
    , domain
    , values
    , expand
    , member
    , toList
    , bindings
    , singleton
    )
    where

import VL.Common
import VL.Expression
import VL.AbstractValue

import qualified VL.Environment as Environment

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map

newtype AbstractAnalysis
    = AbstractAnalysis {
	bindings :: Map (CoreExpression, AbstractEnvironment) AbstractValue
      } deriving Eq

empty :: AbstractAnalysis
empty = AbstractAnalysis Map.empty

union :: AbstractAnalysis -> AbstractAnalysis -> AbstractAnalysis
union a1 a2 = AbstractAnalysis $ Map.union (bindings a1) (bindings a2)

unions :: [AbstractAnalysis] -> AbstractAnalysis
unions = foldl union empty

lookup :: CoreExpression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractValue
lookup e env a = fromMaybe AbstractBottom (Map.lookup (e, env) (bindings a))

insert :: CoreExpression
       -> AbstractEnvironment
       -> AbstractValue
       -> AbstractAnalysis
       -> AbstractAnalysis
insert e env v a = AbstractAnalysis $ Map.insert (e, env) v (bindings a)

domain :: AbstractAnalysis -> [(CoreExpression, AbstractEnvironment)]
domain = Map.keys . bindings

values :: AbstractAnalysis -> [AbstractValue]
-- values a = concat [ v : Environment.values env
-- 		  | ((e, env), v) <- Map.toList (bindings a)
-- 		  ]
values = Map.elems . bindings

expand :: CoreExpression
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractAnalysis
expand e env a
    | (e, env) `member` a
    = empty
    | otherwise
    = singleton e env AbstractBottom

member :: (CoreExpression, AbstractEnvironment) -> AbstractAnalysis -> Bool
member (e, env) a = (e, env) `Map.member` (bindings a)

toList :: AbstractAnalysis -> [((CoreExpression, AbstractEnvironment), AbstractValue)]
toList = Map.toList . bindings

singleton :: CoreExpression
	  -> AbstractEnvironment
	  -> AbstractValue
	  -> AbstractAnalysis
singleton e env v = AbstractAnalysis $ Map.singleton (e, env) v
