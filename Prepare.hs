module VL.Prepare (prepare) where

import VL.Iso
import VL.Syntax
import VL.Expression

import VL.Desugar (desugar)
import VL.Uniquify (uniquify)

-- Uniquification is necessary for the correctness of `pushLetrec'.
prepare :: SurfaceSyntax -> CoreExpr
prepare = iso . uniquify . desugar