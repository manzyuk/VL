module VL where

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as Map

type Name = String

-- Environments

type Environment val = [(Name, val)]

boundVariables :: Environment val -> [Name]
boundVariables env = [x | (x, v) <- env]

lookupVariable :: Name -> Environment val -> val
lookupVariable x env
    = fromMaybe (error $ "Unbound variable: " ++ x) (lookup x env)

extendBindings :: Name -> val -> Environment val -> Environment val
extendBindings x v env = (x, v) : env

-- Expressions

data Expression
    = Variable Name
    | Lambda Name Expression
    | Application Expression Expression
    | Cons Expression Expression
      deriving (Eq, Ord, Show)

-- Concrete values

data Scalar
    = Nil
    | Boolean Bool
    | Real Float
    | Primitive Name
      deriving (Eq, Ord, Show)

data ConcreteValue
    = ConcreteScalar Scalar
    | ConcreteClosure ConcreteEnvironment Name Expression
    | ConcretePair ConcreteValue ConcreteValue
      deriving (Eq, Ord, Show)

type ConcreteEnvironment = Environment ConcreteValue

-- Concrete evaluator

eval :: Expression -> ConcreteEnvironment -> ConcreteValue
eval (Variable x)        env = lookupVariable x env
eval (Lambda x e)        env = ConcreteClosure env x e
eval (Application e1 e2) env = apply (eval e1 env) (eval e2 env)
eval (Cons e1 e2)        env = ConcretePair (eval e1 env) (eval e2 env)

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (extendBindings x v env)
apply _ _ = error "Cannot apply a non-function"

-- Abstract values

data AbstractValue
    = AbstractScalar Scalar
    | AbstractBoolean
    | AbstractReal
    | AbstractClosure AbstractEnvironment Name Expression
    | AbstractPair AbstractValue AbstractValue
    | AbstractTop
      deriving (Eq, Ord, Show)

type AbstractEnvironment = Environment AbstractValue

unifyValues :: AbstractValue -> AbstractValue -> AbstractValue
unifyValues v1 v2
    | v1 == v2
    = v1
unifyValues (AbstractScalar (Boolean b1)) (AbstractScalar (Boolean b2))
    | b1 /= b2
    = AbstractBoolean
unifyValues (AbstractScalar (Boolean _)) AbstractBoolean
    = AbstractBoolean
unifyValues AbstractBoolean (AbstractScalar (Boolean _))
    = AbstractBoolean
unifyValues (AbstractScalar (Real r1)) (AbstractScalar (Real r2))
    | r1 /= r2
    = AbstractReal
unifyValues (AbstractScalar (Real _)) AbstractReal
    = AbstractReal
unifyValues AbstractReal (AbstractScalar (Real _))
    = AbstractReal
unifyValues (AbstractClosure env1 x1 e1) (AbstractClosure env2 x2 e2)
    | x1 == x2 && e1 == e2
    = AbstractClosure (env1 `unifyEnvironments` env2) x1 e1
    where
      -- The environment of a closure is assumed to contain precisely
      -- the free variables of the body, therefore if the bodies are
      -- equal, then the environments must contain the same set of
      -- variables.
      unifyEnvironments env1 env2
          = map unifyBindings (boundVariables env1)
      unifyBindings x
          = (x, (lookupVariable x env1) `unifyValues` (lookupVariable x env2))
unifyValues (AbstractPair v1 v2) (AbstractPair v1' v2')
    = AbstractPair (v1 `unifyValues` v1') (v2 `unifyValues` v2')
unifyValues _ _
    = AbstractTop

-- Abstract analysises

type AbstractAnalysis = Map (Expression, AbstractEnvironment) AbstractValue

-- Abstract evaluator

evalBar1 :: Expression
         -> AbstractEnvironment
         -> AbstractAnalysis
         -> AbstractValue
evalBar1 e env a = fromMaybe AbstractTop (Map.lookup (e, env) a)

applyBar :: AbstractValue
         -> AbstractValue
         -> AbstractAnalysis
         -> AbstractValue
applyBar (AbstractClosure env x e) v a
    | v /= AbstractTop
    = evalBar1 e (extendBindings x v env) a
    | otherwise
    = AbstractTop
applyBar AbstractTop _ _ = AbstractTop
applyBar _ _ _ = error "Cannot apply abstract non-function"

evalBar :: Expression
        -> AbstractEnvironment
        -> AbstractAnalysis
        -> AbstractValue
evalBar (Variable x) env a = lookupVariable x env
evalBar (Lambda x e) env a = AbstractClosure env x e
evalBar (Application e1 e2) env a
    = applyBar (evalBar1 e1 env a) (evalBar1 e2 env a) a
evalBar (Cons e1 e2) env a
    | v1 /= AbstractTop && v2 /= AbstractTop
    = AbstractPair v1 v2
    | otherwise
    = AbstractTop
    where
      v1 = evalBar1 e1 env a
      v2 = evalBar1 e2 env a

evalBar1' :: Expression
          -> AbstractEnvironment
          -> AbstractAnalysis
          -> AbstractAnalysis
evalBar1' e env a
    | (e, env) `Map.member` a
    = Map.singleton (e, env) AbstractTop
    | otherwise
    = Map.empty

applyBar' :: AbstractValue
          -> AbstractValue
          -> AbstractAnalysis
          -> AbstractAnalysis
applyBar' (AbstractClosure env x e) v a
    | v /= AbstractTop
    = evalBar1' e (extendBindings x v env) a
    | otherwise
    = Map.empty
applyBar' AbstractTop _ _ = Map.empty
applyBar' _ _ _ = error "Cannot apply abstract non-function"

evalBar' :: Expression
         -> AbstractEnvironment
         -> AbstractAnalysis
         -> AbstractAnalysis
evalBar' (Variable _) _ _ = Map.empty
evalBar' (Lambda _ _) _ _ = Map.empty
evalBar' (Application e1 e2) env a
    = Map.unions [ evalBar1' e1 env a
                 , evalBar1' e2 env a
                 , applyBar' (evalBar1 e1 env a) (evalBar1 e2 env a) a
                 ]
evalBar' (Cons e1 e2) env a
    = (evalBar1' e1 env a) `Map.union` (evalBar1' e2 env a)

u :: AbstractAnalysis -> AbstractAnalysis
u a = Map.unions . map u1 . Map.keys $ a
    where
      u1 (e, env) = Map.insert (e, env) (evalBar e env a) (evalBar' e env a)

-- NOTE: May not terminate
analyze :: Expression
        -> Environment Scalar   -- Bindings produced by constant conversion
        -> AbstractAnalysis
analyze e constants = leastFixedPoint u a0
    where
      a0 = Map.singleton (e, initialAbstractEnvironment ++ bindings) AbstractTop
      bindings = [(x, AbstractScalar v) | (x, v) <- constants]

leastFixedPoint :: Eq a => (a -> a) -> a -> a
leastFixedPoint f x | f x == x  = x
                    | otherwise = leastFixedPoint f (f x)

initialAbstractEnvironment :: AbstractEnvironment
initialAbstractEnvironment = [] -- will contain bindings for primitives eventually