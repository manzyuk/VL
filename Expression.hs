{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
module VL.Expression where

import VL.Common
import VL.Coproduct

import Control.Arrow (second)

import Data.Set (Set, (\\))
import qualified Data.Set as Set

-- data Expression binder
--     = Variable Name
--     | Lambda binder (Expression binder)
--     | Application (Expression binder) (Expression binder)
--     | Cons (Expression binder) (Expression binder)
--     | Letrec [LocalDefinition binder] (Expression binder)
--       deriving (Eq, Ord, Show)

-- type LocalDefinition binder = (Name, binder, Expression binder)
-- type CoreLocalDefinition = LocalDefinition Name

-- The evaluation rule for `letrec' is based on the following
-- trasformation from Reynolds's "Theories of Programming
-- Languages" (Section 11.3, p. 230):
--
-- letrec v1 = \u1. e1, ..., vn = \un. en in e
--   == (\v1. ... \vn. e) (\u1. e1*) ... (\un. en*)
--
-- where ei* = letrec v1 = \u1. e1, ..., vn = \un. en in ei.
-- transformLetrec :: [CoreLocalDefinition]
--                 -> CoreExpression
--                 -> CoreExpression
-- transformLetrec local_defs body = foldl Application f fs
--     where
--       vs = [v | (v, _, _) <- local_defs]
--       f  = foldr Lambda body vs
--       fs = [Lambda u (Letrec local_defs e) | (_, u, e) <- local_defs]

-- type CoreExpression = Expression Name
-- type SurfaceExpression = Expression [Name]

-- freeVariables :: CoreExpression -> Set Name
-- freeVariables (Variable x) = Set.singleton x
-- freeVariables (Lambda x e) = Set.delete x (freeVariables e)
-- freeVariables (Application e1 e2)
--     = (freeVariables e1) `Set.union` (freeVariables e2)
-- freeVariables (Cons e1 e2)
--     = (freeVariables e1) `Set.union` (freeVariables e2)
-- freeVariables (Letrec ls e)
--     = ((freeVariables e) `Set.union` vs) \\ ns
--     where
--       ns = Set.fromList [n | (n, _, _) <- ls]
--       vs = Set.unions [Set.delete x (freeVariables e) | (_, x, e) <- ls]

--------------------------------------------------------------------------------

data Expr f = In { out :: f (Expr f) }

data Variable            a = Variable Name

-- Lambda expressions
data LambdaOneArg        a = LambdaOneArg Name a
data LambdaManyArgs      a = LambdaManyArgs [Name] a

-- Procedure calls
data ApplicationOneArg   a = ApplicationOneArg a a
data ApplicationManyArgs a = ApplicationManyArgs a [a]

-- Pairs
data Cons                a = Cons a a
data List                a = List [a]
data ConsStar            a = ConsStar [a]

-- Conditionals
data If                  a = If a a a
data Or                  a = Or [a]
data And                 a = And [a]
data Cond                a = Cond [(a, a)]

-- Binding constructs
data Let                 a = Let [(Name, a)] a
data LetrecOneArg        a = LetrecOneArg [(Name, Name, a)] a
data LetrecManyArgs      a = LetrecManyArgs [(Name, [Name], a)] a

-- Functor instances
instance Functor Variable where
    fmap _ (Variable name) = Variable name

instance Functor LambdaOneArg where
    fmap f (LambdaOneArg arg body) = LambdaOneArg arg (f body)

instance Functor LambdaManyArgs where
    fmap f (LambdaManyArgs args body) = LambdaManyArgs args (f body)

instance Functor ApplicationOneArg where
    fmap f (ApplicationOneArg operator operand)
        = ApplicationOneArg (f operator) (f operand)

instance Functor ApplicationManyArgs where
    fmap f (ApplicationManyArgs operator operands)
         = ApplicationManyArgs (f operator) (map f operands)

instance Functor Cons where
    fmap f (Cons x1 x2) = Cons (f x1) (f x2)

instance Functor List where
    fmap f (List xs) = List (map f xs)

instance Functor ConsStar where
    fmap f (ConsStar xs) = ConsStar (map f xs)

instance Functor If where
    fmap f (If predicate consequent alternate)
        = If (f predicate) (f consequent) (f alternate)

instance Functor Or where
    fmap f (Or xs) = Or (map f xs)

instance Functor And where
    fmap f (And xs) = And (map f xs)

instance Functor Let where
    fmap f (Let bindings body)
        = Let (map (second f) bindings) (f body)

instance Functor LetrecOneArg where
    fmap f (LetrecOneArg bindings body)
        = LetrecOneArg (map (third f) bindings) (f body)

instance Functor LetrecManyArgs where
    fmap f (LetrecManyArgs bindings body)
        = LetrecManyArgs (map (third f) bindings) (f body)

third :: (c1 -> c2) -> (a, b, c1) -> (a, b, c2)
third f (x, y, z) = (x, y, f z)

-- Folding over expressions
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

-- Smart constructors
inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

mkVariable :: (Variable :<: f) => Name -> Expr f
mkVariable x = inject (Variable x)

mkLambdaOneArg :: (LambdaOneArg :<: f)
               => Name -> Expr f -> Expr f
mkLambdaOneArg arg body
    = inject (LambdaOneArg arg body)

mkLambdaManyArgs :: (LambdaManyArgs :<: f)
                 => [Name] -> Expr f -> Expr f
mkLambdaManyArgs args body
    = inject (LambdaManyArgs args body)

mkApplicationOneArg :: (ApplicationOneArg :<: f)
                    => Expr f -> Expr f -> Expr f
mkApplicationOneArg operator operand
    = inject (ApplicationOneArg operator operand)

mkApplicationManyArgs :: (ApplicationManyArgs :<: f)
                      => Expr f -> [Expr f] -> Expr f
mkApplicationManyArgs operator operands
    = inject (ApplicationManyArgs operator operands)

mkCons :: (Cons :<: f) => Expr f -> Expr f -> Expr f
mkCons x1 x2 = inject (Cons x1 x2)

mkList :: (List :<: f) => [Expr f] -> Expr f
mkList xs = inject (List xs)

mkConsStar :: (ConsStar :<: f) => [Expr f] -> Expr f
mkConsStar xs = inject (ConsStar xs)

mkIf :: (If :<: f) => Expr f -> Expr f -> Expr f -> Expr f
mkIf predicate consequent alternate
    = inject (If predicate consequent alternate)

mkOr :: (Or :<: f) => [Expr f] -> Expr f
mkOr xs = inject (Or xs)

mkAnd :: (And :<: f) => [Expr f] -> Expr f
mkAnd xs = inject (And xs)

mkCond :: (Cond :<: f) => [(Expr f, Expr f)] -> Expr f
mkCond branches = inject (Cond branches)

mkLet :: (Let :<: f) => [(Name, Expr f)] -> Expr f -> Expr f
mkLet bindings body = inject (Let bindings body)

mkLetrecOneArg :: (LetrecOneArg :<: f)
               => [(Name, Name, Expr f)] -> Expr f -> Expr f
mkLetrecOneArg bindings body
    = inject (LetrecOneArg bindings body)

mkLetrecManyArgs :: (LetrecManyArgs :<: f)
                 => [(Name, [Name], Expr f)] -> Expr f -> Expr f
mkLetrecManyArgs bindings body
    = inject (LetrecManyArgs bindings body)

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
