module VL.Abstract.Analysis
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

import VL.Language.Common
import VL.Language.Pretty hiding (empty)
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Abstract.Value

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map

newtype AbstractAnalysis
    = AbstractAnalysis {
        bindings :: Map (CoreExpr, AbstractEnvironment) AbstractValue
      } deriving Eq

empty :: AbstractAnalysis
empty = AbstractAnalysis Map.empty

union :: AbstractAnalysis -> AbstractAnalysis -> AbstractAnalysis
union a1 a2 = AbstractAnalysis $ Map.union (bindings a1) (bindings a2)

unions :: [AbstractAnalysis] -> AbstractAnalysis
unions = foldl union empty

lookup :: CoreExpr
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractValue
lookup e env a = fromMaybe AbstractBottom (Map.lookup (e, env) (bindings a))

insert :: CoreExpr
       -> AbstractEnvironment
       -> AbstractValue
       -> AbstractAnalysis
       -> AbstractAnalysis
insert e env v a = AbstractAnalysis $ Map.insert (e, env) v (bindings a)

domain :: AbstractAnalysis -> [(CoreExpr, AbstractEnvironment)]
domain = Map.keys . bindings

values :: AbstractAnalysis -> [AbstractValue]
--values = Map.elems . bindings
values a = concat [ v : Environment.values (Environment.restrict (freeVariables e) env)
                  | ((e, env), v) <- toList a
                  ]

expand :: CoreExpr
       -> AbstractEnvironment
       -> AbstractAnalysis
       -> AbstractAnalysis
expand e env a
    | (e, env) `member` a
    = empty
    | otherwise
    = singleton e env AbstractBottom

member :: (CoreExpr, AbstractEnvironment) -> AbstractAnalysis -> Bool
member (e, env) a = (e, env) `Map.member` (bindings a)

toList :: AbstractAnalysis -> [((CoreExpr, AbstractEnvironment), AbstractValue)]
toList = Map.toList . bindings

singleton :: CoreExpr
          -> AbstractEnvironment
          -> AbstractValue
          -> AbstractAnalysis
singleton e env v = AbstractAnalysis $ Map.singleton (e, env) v

-- Pretty-printing of analyses
instance Pretty AbstractAnalysis where
    pp analysis = internal "analysis"
                . vcat
                . punctuate newline
                . map ppBinding
                . toList
                $ analysis
        where
          ppBinding ((e, env), v) = sep [ ppPair e env
                                        , text "==>"
                                        , pp v
                                        ]
