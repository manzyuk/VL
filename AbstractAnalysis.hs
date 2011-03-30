module VL.AbstractAnalysis where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment
import VL.AbstractValue

import Data.Map (Map)
import qualified Data.Map as Map

type AbstractAnalysis = Map (Expression, AbstractEnvironment) AbstractValue

unifyAnalyses :: [AbstractAnalysis] -> AbstractAnalysis
unifyAnalyses = foldl (Map.unionWith unifyValues) Map.empty
