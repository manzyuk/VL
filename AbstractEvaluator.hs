module VL.AbstractEvaluator where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment
import VL.AbstractValue
import VL.AbstractAnalysis

import Data.Map (Map)
import qualified Data.Map as Map

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
evalBar e@(Lambda x b) env a = AbstractClosure env' x b
    where
      env' = restrictDomain (freeVariables e) env
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
    = unifyAnalyses [ evalBar1' e1 env a
                    , evalBar1' e2 env a
                    , applyBar' (evalBar1 e1 env a) (evalBar1 e2 env a) a
                    ]
evalBar' (Cons e1 e2) env a
    = unifyAnalyses [ evalBar1' e1 env a
                    , evalBar1' e2 env a
                    ]

u :: AbstractAnalysis -> AbstractAnalysis
u a = unifyAnalyses . map u1 . Map.keys $ a
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