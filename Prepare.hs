module VL.Prepare (prepare) where

import VL.Expression

import VL.Desugar (desugar)
import VL.Uniquify (uniquify)

-- Uniquification is necessary for the correctness of `pushLetrec'.
prepare :: SurfaceExpression -> CoreExpression
prepare = uniquify . desugar