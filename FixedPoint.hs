{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell                                #-}
module VL.FixedPoint where

import VL.Coproduct

import Control.Monad
import Language.Haskell.TH

newtype Fix f = In { out :: f (Fix f) }

-- StandaloneDeriving extension allows us to derive Eq and Ord
-- instances for Expr f.  I've borrowed this idea from
--
-- http://mainisusuallyafunction.blogspot.com/2010/12/type-level-fix-and-generic-folds.html
--
-- See also
--
-- http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/deriving.html#stand-alone-deriving
--
-- Thanks to DeriveFunctor extension we can get rid of another
-- chunk of boilerplate.

deriving instance (Eq  (f (Fix f))) => Eq  (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

-- Catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f (In t) = f (fmap (cata f) t)

-- Injections into the fixed point
inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

-- Define smart constructors for a list of given type names.  The name
-- of the smart constructor is obtained by prefixing the name of the
-- corresponding data constructor with "mk".  The smart constructor
-- applies the data constructor to its arguments and injects the
-- result into 'Fix' using 'inject'.
defineSmartConstructors :: [Name] -> Q [Dec]
defineSmartConstructors = liftM concat . mapM defineSCs

defineSCs :: Name -> Q [Dec]
defineSCs name = do
  TyConI (DataD _ _ _ cs _) <- reify name
  mapM defineSC cs

defineSC :: Con -> Q Dec
defineSC (NormalC name args) = funD name' clauses
    where
      name'   = mkName $ "mk" ++ nameBase name
      clauses = [clause (map varP vars) body []]
      vars    = [mkName $ "x" ++ show i | (i, _) <- zip [1..] args]
      body    = normalB (appE (varE 'inject)
			      (foldl appE (conE name) (map varE vars)))
