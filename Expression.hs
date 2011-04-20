{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell                                #-}
module VL.Expression where

import VL.Common
import VL.Coproduct
import VL.FixedPoint

import Data.Set (Set, (\\))
import qualified Data.Set as Set

type Expr = Fix

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

-- Folding over expressions
foldExpr :: Functor f => (f a -> a) -> Fix f -> a
foldExpr = cata

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

-- Variables
variables :: Variables f => Expr f -> Set Name
variables = foldExpr variablesAlg

class Functor f => Variables f where
    variablesAlg :: f (Set Name) -> Set Name

instance Variables Variable where
    variablesAlg (Variable x) = Set.singleton x

instance Variables LambdaOneArg where
    variablesAlg (LambdaOneArg arg body)
        = Set.insert arg body

instance Variables LambdaManyArgs where
    variablesAlg (LambdaManyArgs args body)
        =  (Set.fromList args) `Set.union` body

instance Variables ApplicationOneArg where
    variablesAlg (ApplicationOneArg operator operand)
        = operator `Set.union` operand

instance Variables ApplicationManyArgs where
    variablesAlg (ApplicationManyArgs operator operands)
        = Set.unions (operator : operands)

instance Variables Cons where
    variablesAlg (Cons x1 x2) = x1 `Set.union` x2

instance Variables List where
    variablesAlg (List xs) = Set.unions xs

instance Variables ConsStar where
    variablesAlg (ConsStar xs) = Set.unions xs

instance Variables If where
    variablesAlg (If predicate consequent alternate)
        = Set.unions [predicate, consequent, alternate]

instance Variables Or where
    variablesAlg (Or xs) = Set.unions xs

instance Variables And where
    variablesAlg (And xs) = Set.unions xs

instance Variables Not where
    variablesAlg (Not x) = x

instance Variables Let where
    variablesAlg (Let bindings body)
        = body `Set.union` vs `Set.union` ns
        where
          ns = Set.fromList [ name
                            | (name, _) <- bindings ]
          vs = Set.unions   [ expr
                            | (_, expr) <- bindings ]

instance Variables LetrecOneArg where
    variablesAlg (LetrecOneArg bindings body)
        = body `Set.union` vs `Set.union` ns
        where
          ns = Set.fromList [ name
                            | (name, _, _)   <- bindings ]
          vs = Set.unions   [ Set.insert arg body
                            | (_, arg, body) <- bindings ]

instance Variables LetrecManyArgs where
    variablesAlg (LetrecManyArgs bindings body)
        = body `Set.union` vs `Set.union` ns
        where
          ns = Set.fromList [ name
                            | (name, _, _)    <- bindings ]
          vs = Set.unions   [ (Set.fromList args) `Set.union` body
                            | (_, args, body) <- bindings ]

instance (Variables f, Variables g) => Variables (f :+: g) where
    variablesAlg (Inl x) = variablesAlg x
    variablesAlg (Inr x) = variablesAlg x


-- Free variables
freeVariables :: FreeVariables f => Expr f -> Set Name
freeVariables = foldExpr freeVariablesAlg

class Functor f => FreeVariables f where
    freeVariablesAlg :: f (Set Name) -> Set Name

instance FreeVariables Variable where
    freeVariablesAlg (Variable x) = Set.singleton x

instance FreeVariables LambdaOneArg where
    freeVariablesAlg (LambdaOneArg arg body)
        = Set.delete arg body

instance FreeVariables LambdaManyArgs where
    freeVariablesAlg (LambdaManyArgs args body)
        = body \\ (Set.fromList args)

instance FreeVariables ApplicationOneArg where
    freeVariablesAlg (ApplicationOneArg operator operand)
        = operator `Set.union` operand

instance FreeVariables ApplicationManyArgs where
    freeVariablesAlg (ApplicationManyArgs operator operands)
        = Set.unions (operator : operands)

instance FreeVariables Cons where
    freeVariablesAlg (Cons x1 x2) = x1 `Set.union` x2

instance FreeVariables List where
    freeVariablesAlg (List xs) = Set.unions xs

instance FreeVariables ConsStar where
    freeVariablesAlg (ConsStar xs) = Set.unions xs

instance FreeVariables If where
    freeVariablesAlg (If predicate consequent alternate)
        = Set.unions [predicate, consequent, alternate]

instance FreeVariables Or where
    freeVariablesAlg (Or xs) = Set.unions xs

instance FreeVariables And where
    freeVariablesAlg (And xs) = Set.unions xs

instance FreeVariables Not where
    freeVariablesAlg (Not x) = x

instance FreeVariables Let where
    freeVariablesAlg (Let bindings body)
        = (body `Set.union` vs) \\ ns
        where
          ns = Set.fromList [ name
                            | (name, _) <- bindings ]
          vs = Set.unions   [ expr
                            | (_, expr) <- bindings ]

instance FreeVariables LetrecOneArg where
    freeVariablesAlg (LetrecOneArg bindings body)
        = (body `Set.union` vs) \\ ns
        where
          ns = Set.fromList [ name
                            | (name, _, _)   <- bindings ]
          vs = Set.unions   [ Set.delete arg body
                            | (_, arg, body) <- bindings ]

instance FreeVariables LetrecManyArgs where
    freeVariablesAlg (LetrecManyArgs bindings body)
        = (body `Set.union` vs) \\ ns
        where
          ns = Set.fromList [ name
                            | (name, _, _)    <- bindings ]
          vs = Set.unions   [ body \\ (Set.fromList args)
                            | (_, args, body) <- bindings ]

instance (FreeVariables f, FreeVariables g) => FreeVariables (f :+: g) where
    freeVariablesAlg (Inl x) = freeVariablesAlg x
    freeVariablesAlg (Inr x) = freeVariablesAlg x

-- Type synonyms for the most important expression types
type Core
    =     Variable
      :+: LambdaOneArg
      :+: ApplicationOneArg
      :+: Cons
      :+: LetrecOneArg

type CoreExpression = Expr Core

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

type SurfaceExpression = Expr Surface

-- The evaluation rule for `letrec' is based on the following
-- trasformation from Reynolds's "Theories of Programming
-- Languages" (Section 11.3, p. 230):
--
-- letrec v1 = \u1. e1, ..., vn = \un. en in e
--   == (\v1. ... \vn. e) (\u1. e1*) ... (\un. en*)
--
-- where ei* = letrec v1 = \u1. e1, ..., vn = \un. en in ei.
pushLetrec :: (ApplicationOneArg :<: f, LambdaOneArg :<: f, LetrecOneArg :<: f)
           => [(Name, Name, Expr f)] -> Expr f -> Expr f
pushLetrec bindings body = foldl mkApplicationOneArg f fs
    where
      vs = [ v | (v, _, _) <- bindings ]
      f  = foldr mkLambdaOneArg body vs
      fs = [ mkLambdaOneArg u (mkLetrecOneArg bindings e)
           | (_, u, e) <- bindings
           ]
