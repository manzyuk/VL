module VL.ConcreteEvaluator where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment
import VL.ConcreteValue

eval :: Expression -> ConcreteEnvironment -> ConcreteValue
eval (Variable x)        env = lookupVariable x env
eval e@(Lambda x b)      env = ConcreteClosure env' x b
    where
      env' = restrictDomain (freeVariables e) env
eval (Application e1 e2) env = apply (eval e1 env) (eval e2 env)
eval (Cons e1 e2)        env = ConcretePair (eval e1 env) (eval e2 env)

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (extendBindings x v env)
apply _ _ = error "Cannot apply a non-function"
