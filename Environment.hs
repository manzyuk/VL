module VL.Environment where

import VL.Common

import Data.Set (Set)
import qualified Data.Set as Set

type Environment val = [(Name, val)]

boundVariables :: Environment val -> [Name]
boundVariables env = [x | (x, v) <- env]

lookupVariable :: Name -> Environment val -> val
lookupVariable x env
    = fromMaybe (error $ "Unbound variable: " ++ x) (lookup x env)

extendBindings :: Name -> val -> Environment val -> Environment val
extendBindings x v env = (x, v) : env

restrictDomain :: Set Name -> Environment val -> Environment val
restrictDomain set env = [(x, v) | (x, v) <- env, x `Set.member` set]
