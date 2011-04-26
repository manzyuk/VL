module VL.Language.Rename
    ( Supply
    , maybeRename
    , freshName
    , evalSupply
    , alphaRename
    )
    where

import VL.Language.Common
import VL.Language.Expression

import VL.Alacarte.Coproduct

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import Control.Monad
import Control.Monad.State

type Dictionary = Map Name Name

maybeRename :: Dictionary -> Name -> Name
maybeRename dict x = fromMaybe x (Map.lookup x dict)

-- Rename the variables of an expression according to a supplied
-- dictionary, leaving unchanged the variables w/out translation.
rename :: Dictionary -> CoreExpr -> CoreExpr
rename dict (Var x)                = Var x'
    where
      x' = maybeRename dict x
rename dict (Lam x e)              = Lam x' e'
    where
      x' = maybeRename dict x
      e' = rename dict e
rename dict (App e1 e2)            = App e1' e2'
    where
      e1' = rename dict e1
      e2' = rename dict e2
rename dict (Pair e1 e2)           = Pair e1' e2'
    where
      e1' = rename dict e1
      e2' = rename dict e2
rename dict (Letrec bindings body) = Letrec bindings' body'
    where
      bindings' = [ (maybeRename dict v, maybeRename dict u, rename dict e)
                  | (v, u, e) <- bindings
                  ]
      body'     = rename dict body

type Supply = State Int

evalSupply :: Supply a -> a
evalSupply = flip evalState 0

freshName :: Name -> Supply Name
freshName prefix = do i <- get
                      let name = prefix ++ show i
                      put (succ i)
                      return name

freshVarName :: Supply Name
freshVarName = freshName "#:var-"

-- Consistently rename bound variables of an expression with unique
-- fresh names.
renameBound :: CoreExpr -> Supply CoreExpr
renameBound e@(Var x)    = return e
renameBound (Lam x e)    = do (x', e') <- renameBoundLambda x e
                              return $ Lam x' e'
renameBound (App e1 e2)  = liftM2 App  (renameBound e1) (renameBound e2)
renameBound (Pair e1 e2) = liftM2 Pair (renameBound e1) (renameBound e2)
renameBound (Letrec bindings body)
    = do vs' <- sequence [ freshVarName          | _      <- vs ]
         ls' <- sequence [ renameBoundLambda u e | (u, e) <- ls ]
         let dict = Map.fromList $ zip vs vs'
             bindings' = [ (v', u', rename dict e')
                         | (v', (u', e')) <- zip vs' ls'
                         ]
         body' <- renameBound $ rename dict body
         return $ Letrec bindings' body'
    where
      vs = [ v      | (v, _, _) <- bindings ]
      ls = [ (u, e) | (_, u, e) <- bindings ]

renameBoundLambda :: Name -> CoreExpr -> Supply (Name, CoreExpr)
renameBoundLambda x e
    = do x' <- freshVarName
         e' <- renameBound $ rename (Map.singleton x x') e
         return (x', e')

-- Assuming that bound variables have been renamed, rename free
-- variables with unique fresh names.  Return the new expression
-- and a dictionary containing translations of the free variables.
renameFree :: CoreExpr -> Supply (CoreExpr, Dictionary)
renameFree expr = do fvs' <- sequence [ freshVarName | _ <- fvs ]
                     let dict = Map.fromList $ zip fvs fvs'
                     return (rename dict expr, dict)
    where
      fvs = Set.toList $ freeVariables expr

alphaRename :: CoreExpr -> (CoreExpr, Dictionary)
alphaRename = evalSupply . (renameFree <=< renameBound)