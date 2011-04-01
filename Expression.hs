module VL.Expression where

import VL.Common

import Data.Set (Set)
import qualified Data.Set as Set

data Expression binder
    = Variable Name
    | Lambda binder (Expression binder)
    | Application (Expression binder) (Expression binder)
    | Cons (Expression binder) (Expression binder)
      deriving (Eq, Ord, Show)

type CoreExpression = Expression Name
type SurfaceExpression = Expression [Name]

freeVariables :: CoreExpression -> Set Name
freeVariables (Variable x) = Set.singleton x
freeVariables (Lambda x e) = Set.delete x (freeVariables e)
freeVariables (Application e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
freeVariables (Cons e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
