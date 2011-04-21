{-# LANGUAGE TypeOperators #-}
module VL.Language.Iso (iso, osi) where

import VL.Language.Syntax
import VL.Language.Expression

import VL.Alacarte.Coproduct
import VL.Alacarte.FixedPoint

-- The types 'CoreSyntax' and 'CoreExpr' are isomorphic, however the
-- latter is easier to work with.  The function 'iso' establishes an
-- isomorphism.  Its inverse is given by the function 'osi' below.
iso :: CoreSyntax -> CoreExpr
iso = foldSyntax isoAlg

class Functor f => Iso f where
    isoAlg :: f CoreExpr -> CoreExpr

instance Iso Variable where
    isoAlg (Variable x) = Var x

instance Iso LambdaOneArg where
    isoAlg (LambdaOneArg formal body) = Lam formal body

instance Iso ApplicationOneArg where
    isoAlg (ApplicationOneArg operator operand) = App operator operand

instance Iso Cons where
    isoAlg (Cons car cdr) = Pair car cdr

instance Iso LetrecOneArg where
    isoAlg (LetrecOneArg bindings body) = Letrec bindings body

instance (Iso f, Iso g) => Iso (f :+: g) where
    isoAlg (Inl x) = isoAlg x
    isoAlg (Inr x) = isoAlg x

osi :: CoreExpr -> CoreSyntax
osi (Var x)
    = mkVariable x
osi (Lam formal body)
    = mkLambdaOneArg formal (osi body)
osi (App operator operand)
    = mkApplicationOneArg (osi operator) (osi operand)
osi (Pair car cdr)
    = mkCons (osi car) (osi cdr)
osi (Letrec bindings body)
    = mkLetrecOneArg [ (v, u, osi e) | (v, u, e) <- bindings ] (osi body)
