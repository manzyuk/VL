module VL.Expression where

import VL.Common

import Data.Set (Set, (\\))
import qualified Data.Set as Set

data Expression binder
    = Variable Name
    | Lambda binder (Expression binder)
    | Application (Expression binder) (Expression binder)
    | Cons (Expression binder) (Expression binder)
    | Letrec [LocalDefinition binder] (Expression binder)
      deriving (Eq, Ord, Show)

type LocalDefinition binder = (Name, binder, Expression binder)
type CoreLocalDefinition = LocalDefinition Name

-- The evaluation rule for `letrec' is based on the following
-- trasformation from Reynolds's "Theories of Programming
-- Languages" (Section 11.3, p. 230):
--
-- letrec v1 = \u1. e1, ..., vn = \un. en in e
--   == (\v1. ... \vn. e) (\u1. e1*) ... (\un. en*)
--
-- where ei* = letrec v1 = \u1. e1, ..., vn = \un. en in ei.
transformLetrec :: [CoreLocalDefinition]
                -> CoreExpression
                -> CoreExpression
transformLetrec local_defs body = foldl Application f fs
    where
      vs = [v | (v, _, _) <- local_defs]
      f  = foldr Lambda body vs
      fs = [Lambda u (Letrec local_defs e) | (_, u, e) <- local_defs]

type CoreExpression = Expression Name
type SurfaceExpression = Expression [Name]

freeVariables :: CoreExpression -> Set Name
freeVariables (Variable x) = Set.singleton x
freeVariables (Lambda x e) = Set.delete x (freeVariables e)
freeVariables (Application e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
freeVariables (Cons e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
freeVariables (Letrec ls e)
    = ((freeVariables e) `Set.union` vs) \\ ns
    where
      ns = Set.fromList [n | (n, _, _) <- ls]
      vs = Set.unions [Set.delete x (freeVariables e) | (_, x, e) <- ls]
