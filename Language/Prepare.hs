module VL.Language.Prepare (prepare) where

import VL.Language.Iso
import VL.Language.Syntax
import VL.Language.Expression

import VL.Language.Desugar
import VL.Language.Uniquify

-- Uniquification is necessary for the correctness of `pushLetrec'.
prepare :: SurfaceSyntax -> CoreExpr
prepare = iso . uniquify . desugar