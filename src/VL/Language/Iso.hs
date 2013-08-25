{-# LANGUAGE TypeOperators #-}
module VL.Language.Iso (iso) where

import VL.Language.Syntax
import VL.Language.Expression

import VL.Alacarte.Coproduct

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
