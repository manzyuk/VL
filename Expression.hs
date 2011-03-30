module VL.Expression where

import VL.Common

import Data.Set (Set)
import qualified Data.Set as Set

data Expression
    = Variable Name
    | Lambda Name Expression
    | Application Expression Expression
    | Cons Expression Expression
      deriving (Eq, Ord, Show)

freeVariables :: Expression -> Set Name
freeVariables (Variable x) = Set.singleton x
freeVariables (Lambda x e) = Set.delete x (freeVariables e)
freeVariables (Application e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
freeVariables (Cons e1 e2)
    = (freeVariables e1) `Set.union` (freeVariables e2)
