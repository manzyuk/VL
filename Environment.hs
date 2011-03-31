module VL.Environment where

import VL.Common

import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

type Environment val = [(Name, val)]

empty :: Environment val
empty = []

union :: Eq val => Environment val -> Environment val -> Environment val
union = List.union

domain :: Environment val -> [Name]
domain env = [x | (x, v) <- env]

lookup :: Name -> Environment val -> val
lookup x env
    = fromMaybe (error $ "Unbound variable: " ++ x) (List.lookup x env)

insert :: Name -> val -> Environment val -> Environment val
insert x v env = (x, v) : env

restrict :: Set Name -> Environment val -> Environment val
restrict set env = [(x, v) | (x, v) <- env, x `Set.member` set]

singleton :: Name -> val -> Environment val
singleton x v = [(x, v)]
