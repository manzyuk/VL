{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell                                #-}
module VL.Alacarte.FixedPoint where

import VL.Alacarte.Coproduct

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

-- Template Haskell macros

-- Define smart constructors for a list of given type names.
--
-- The smart constructor applies the data constructor to its arguments
-- and injects the result into 'Fix' using 'inject'.
--
-- We adopt the convention that the name of the smart constructor is
-- obtained from the name of the corresponding data constructor by
-- prefixing the latter with "mk".
defineSmartConstructors :: [Name] -> Q [Dec]
defineSmartConstructors = liftM concat . mapM defineSCs

defineSCs :: Name -> Q [Dec]
defineSCs name = do
  TyConI (DataD _ _ _ cs _) <- reify name
  mapM defineSC cs

defineSC :: Con -> Q Dec
defineSC con = funD name' clauses
    where
      (name, name', vars) = generateNames con
      clauses             = [clause (map varP vars) body []]
      body                = normalB (appE (varE 'inject)
                                     (foldl appE (conE name) (map varE vars)))

-- Derive instances of a class for a list of given type names.
--
-- We assume that the class Foo whose name is supplied as the first
-- argument to 'deriveAlgebraInstances' is of the form:
--
-- class Functor f => Foo f where
--    bar :: f Baz -> Baz
--
-- where Baz is a type of the form Fix S, where S is a coproduct of
-- some types, and the types whose names are supplied as the second
-- argument to 'deriveAlgebraInstances' are assumed to occur as
-- direct summands in S.  For such types, there is a trivial
-- instance of Foo:
--
-- instance Foo C where
--    bar (C x1 x2 ... xn) = mkC x1 x2 ... xn
--
-- which is what 'deriveAlgebraInstances' generates.
deriveAlgebraInstances :: Name -> [Name] -> Q [Dec]
deriveAlgebraInstances = mapM . deriveAlgebraInstance

deriveAlgebraInstance :: Name -> Name -> Q Dec
deriveAlgebraInstance cl ty = do
  ClassI (ClassD _ _ _ _ [SigD name _]) _ <- reify cl
  TyConI (DataD  _ _ _ cs _)              <- reify ty
  instanceD (cxt []) (appT (conT cl) (conT ty)) (map (defineAlgebra name) cs)

defineAlgebra :: Name -> Con -> Q Dec
defineAlgebra algebra con = funD algebra clauses
    where
      (name, name', vars) = generateNames con
      clauses             = [clause [conP name (map varP vars)] body []]
      body                = normalB (foldl appE (varE name') (map varE vars))

generateNames :: Con -> (Name, Name, [Name])
generateNames (NormalC name args) = (name, name', vars)
    where
      name' = mkName ('m' : 'k' : nameBase name)
      vars  = [mkName ('x' : show i) | (i, _) <- zip [(1 :: Integer)..] args]
generateNames _ = error "generateNames: the argument is not a normal constructor"