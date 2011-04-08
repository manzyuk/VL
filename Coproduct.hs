{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
module VL.Coproduct where

data (f :+: g) a = Inl (f a) | Inr (g a) deriving (Eq, Ord)
infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl (fmap f x)
    fmap f (Inr x) = Inr (fmap f x)

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

-- GHC wouldn't allow us to write the following instances in a more
-- natural style, e.g., f :<: f, or f :<: (f :+: g).
instance Functor f => (:<:) f f where
    inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => (:<:) f (h :+: g) where
    inj = Inr . inj
