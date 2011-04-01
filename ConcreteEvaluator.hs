module VL.ConcreteEvaluator where

import VL.Expression

import qualified VL.Environment as Environment

import VL.ConcreteValue

eval :: CoreExpression -> ConcreteEnvironment -> ConcreteValue
eval (Variable x)        env = Environment.lookup x env
eval e@(Lambda x b)      env = ConcreteClosure env' x b
    where
      env' = Environment.restrict (freeVariables e) env
eval (Application e1 e2) env = apply (eval e1 env) (eval e2 env)
eval (Cons e1 e2)        env = ConcretePair (eval e1 env) (eval e2 env)

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (Environment.insert x v env)
apply _ _ = error "Cannot apply a concrete non-function"
