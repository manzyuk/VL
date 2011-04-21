{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell                                #-}
module VL.Language.Syntax where

import VL.Language.Common

import VL.Alacarte.Coproduct
import VL.Alacarte.FixedPoint

import Data.Set (Set, (\\))
import qualified Data.Set as Set

type Syntax = Fix

data Variable            a = Variable Name
			     deriving (Eq, Ord, Functor)

-- Lambda expressions
data LambdaOneArg        a = LambdaOneArg Name a
			     deriving (Eq, Ord, Functor)
data LambdaManyArgs      a = LambdaManyArgs [Name] a
			     deriving (Eq, Ord, Functor)

-- Procedure calls
data ApplicationOneArg   a = ApplicationOneArg a a
			     deriving (Eq, Ord, Functor)
data ApplicationManyArgs a = ApplicationManyArgs a [a]
			     deriving (Eq, Ord, Functor)

-- Pairs
data Cons                a = Cons a a
			     deriving (Eq, Ord, Functor)
data List                a = List [a]
			     deriving (Eq, Ord, Functor)
data ConsStar            a = ConsStar [a]
			     deriving (Eq, Ord, Functor)

-- Conditionals
data If                  a = If a a a
			     deriving (Eq, Ord, Functor)
data Or                  a = Or [a]
			     deriving (Eq, Ord, Functor)
data And                 a = And [a]
			     deriving (Eq, Ord, Functor)
data Not                 a = Not a
			     deriving (Eq, Ord, Functor)
data Cond                a = Cond [(a, a)]
			     deriving (Eq, Ord, Functor)

-- Binding constructs
data Let                 a = Let [(Name, a)] a
			     deriving (Eq, Ord, Functor)
data LetrecOneArg        a = LetrecOneArg [(Name, Name, a)] a
			     deriving (Eq, Ord, Functor)
data LetrecManyArgs      a = LetrecManyArgs [(Name, [Name], a)] a
			     deriving (Eq, Ord, Functor)

-- Folding over a syntax
foldSyntax :: Functor f => (f a -> a) -> Syntax f -> a
foldSyntax = cata

-- Smart constructors
$(defineSmartConstructors [ ''Variable
			  , ''LambdaOneArg
			  , ''LambdaManyArgs
			  , ''ApplicationOneArg
			  , ''ApplicationManyArgs
			  , ''Cons
			  , ''List
			  , ''ConsStar
			  , ''If
			  , ''Or
			  , ''And
			  , ''Not
			  , ''Cond
			  , ''Let
			  , ''LetrecOneArg
			  , ''LetrecManyArgs
			  ])

-- Type synonyms for the most important syntax types
type Core
    =     Variable
      :+: LambdaOneArg
      :+: ApplicationOneArg
      :+: Cons
      :+: LetrecOneArg

type CoreSyntax = Syntax Core

type Surface
    =     Variable
      :+: LambdaManyArgs
      :+: ApplicationManyArgs
      :+: Cons
      :+: List
      :+: ConsStar
      :+: If
      :+: Or
      :+: And
      :+: Not
      :+: Cond
      :+: Let
      :+: LetrecManyArgs

type SurfaceSyntax = Syntax Surface
