module VL.Language.Read (read) where

import VL.Language.Iso
import VL.Language.Scalar
import VL.Language.Syntax
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Language.Parser
import VL.Language.Rename
import VL.Language.Desugar

import Prelude hiding (read)

-- Parse an expression from a string and returne a desugared,
-- alpha-renamed core expression together with an environment
-- containing bindings for the constants and primitives that
-- occur in the expression.
read :: String -> (CoreExpr, ScalarEnvironment)
read input = (expression, environment)
    where
      (syntax, constants) = parse input
      (expression, dict)  = alphaRename . iso . desugar $ syntax
      environment         = Environment.restrict (freeVariables expression)
                          . Environment.mapKeys (maybeRename dict)
                          $ primitives `Environment.union` constants
