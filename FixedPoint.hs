{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
module VL.FixedPoint where

import VL.Coproduct

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
