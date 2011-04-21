{-# LANGUAGE TypeOperators #-}
module VL.Uniquify
    ( Supply
    , evalSupply
    , freshName
    , uniquify
    )
    where

import VL.Common
import VL.Syntax
import VL.Coproduct
import VL.FixedPoint

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

-- Variable renaming
type Dictionary = Map Name Name

rename :: Dictionary -> CoreSyntax -> CoreSyntax
rename dict = foldSyntax (renameAlg dict)

class Functor f => Rename f where
    renameAlg :: Dictionary -> f CoreSyntax -> CoreSyntax

maybeRename :: Dictionary -> Name -> Name
maybeRename dict name = fromMaybe name (Map.lookup name dict)

instance Rename Variable where
    renameAlg dict (Variable x) = mkVariable x'
	where
	  x' = maybeRename dict x

instance Rename LambdaOneArg where
    renameAlg dict (LambdaOneArg arg body) = mkLambdaOneArg arg' body
	where
	  arg' = maybeRename dict arg

instance Rename ApplicationOneArg where
    renameAlg dict (ApplicationOneArg operator operand)
	= mkApplicationOneArg operator operand

instance Rename Cons where
    renameAlg dict (Cons e1 e2) = mkCons e1 e2

instance Rename LetrecOneArg where
    renameAlg dict (LetrecOneArg bindings body)
	= mkLetrecOneArg bindings' body
	where
	  bindings' = [ ( maybeRename dict v
			, maybeRename dict u
			, e
			)
		      | (v, u, e) <- bindings
		      ]

instance (Rename f, Rename g) => Rename (f :+: g) where
    renameAlg dict (Inl x) = renameAlg dict x
    renameAlg dict (Inr x) = renameAlg dict x

-- Uniquification: making bound variables unique
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

uniquify :: CoreSyntax -> CoreSyntax
uniquify = evalSupply . foldSyntax uniquifyAlg

class Functor f => Uniquify f where
    uniquifyAlg :: f (Supply CoreSyntax) -> Supply CoreSyntax

instance Uniquify Variable where
    uniquifyAlg (Variable x) = return (mkVariable x)

instance Uniquify LambdaOneArg where
    uniquifyAlg (LambdaOneArg arg body)
	= do (arg', body') <- uniquifyLambda arg body
	     return $ mkLambdaOneArg arg' body'

uniquifyLambda :: Name -> Supply CoreSyntax -> Supply (Name, CoreSyntax)
uniquifyLambda arg body
    = do x <- freshVarName
	 b <- body
	 return (x, rename (Map.singleton arg x) b)

instance Uniquify ApplicationOneArg where
    uniquifyAlg (ApplicationOneArg operator operand)
	= liftM2 mkApplicationOneArg operator operand

instance Uniquify Cons where
    uniquifyAlg (Cons e1 e2) = liftM2 mkCons e1 e2

instance Uniquify LetrecOneArg where
    uniquifyAlg (LetrecOneArg bindings body)
	= do vs' <- sequence [ freshVarName       | v      <- vs ]
	     ls' <- sequence [ uniquifyLambda u e | (u, e) <- ls ]
	     b   <- body
	     let dict = Map.fromList $ zip vs vs'
		 bindings' = [ (v', u', rename dict e')
			     | (v', (u', e')) <- zip vs' ls'
			     ]
		 b' = rename dict b
	     return $ mkLetrecOneArg bindings' b'
	where
	  vs = [ v      | (v, _, _) <- bindings ]
	  ls = [ (u, e) | (_, u, e) <- bindings ]

instance (Uniquify f, Uniquify g) => Uniquify (f :+: g) where
    uniquifyAlg (Inl x) = uniquifyAlg x
    uniquifyAlg (Inr x) = uniquifyAlg x
