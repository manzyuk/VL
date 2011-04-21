module VL.Expression where

import VL.Common

import Data.Set (Set, (\\))
import qualified Data.Set as Set

data CoreExpr
    = Var Name
    | Lam Name CoreExpr
    | App CoreExpr CoreExpr
    | Pair CoreExpr CoreExpr
    | Letrec [LetrecBinding] CoreExpr
      deriving (Eq, Ord, Show)

type LetrecBinding = (Name, Name, CoreExpr)

variables :: CoreExpr -> Set Name
variables (Var x)
    = Set.singleton x
variables (Lam formal body)
    = Set.insert formal (variables body)
variables (App operator operand)
    = (variables operator) `Set.union` (variables operand)
variables (Pair car cdr)
    = (variables car) `Set.union` (variables cdr)
variables (Letrec bindings body)
    = (variables body) `Set.union` vs `Set.union` ns
    where
      ns = Set.fromList [ name | (name, _, _)      <- bindings ]
      vs = Set.unions   [ Set.insert formal (variables body)
			       | (_, formal, body) <- bindings ]

freeVariables :: CoreExpr -> Set Name
freeVariables (Var x)
    = Set.singleton x
freeVariables (Lam formal body)
    = Set.delete formal (freeVariables body)
freeVariables (App operator operand)
    = (freeVariables operator) `Set.union` (freeVariables operand)
freeVariables (Pair car cdr)
    = (freeVariables car) `Set.union` (freeVariables cdr)
freeVariables (Letrec bindings body)
    = (freeVariables body `Set.union` vs) \\ ns
    where
      ns = Set.fromList [ name
			| (name, _, _)      <- bindings ]
      vs = Set.unions   [ Set.delete formal (freeVariables body)
			| (_, formal, body) <- bindings ]

-- The evaluation rule for `letrec' is based on the following
-- trasformation from Reynolds's "Theories of Programming
-- Languages" (Section 11.3, p. 230):
--
-- letrec v1 = \u1. e1, ..., vn = \un. en in e
--   == (\v1. ... \vn. e) (\u1. e1*) ... (\un. en*)
--
-- where ei* = letrec v1 = \u1. e1, ..., vn = \un. en in ei.
pushLetrec :: [(Name, Name, CoreExpr)] -> CoreExpr -> CoreExpr
pushLetrec bindings body = foldl App f fs
    where
      vs = [ v | (v, _, _) <- bindings ]
      f  = foldr Lam body vs
      fs = [ Lam u (Letrec bindings e)
	       | (_, u, e) <- bindings ]
