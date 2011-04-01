module VL.AbstractEvaluator where

import VL.Common
import VL.Scalar
import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.AbstractValue

import VL.AbstractAnalysis (AbstractAnalysis)
import qualified VL.AbstractAnalysis as Analysis

refineApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractValue
refineApply (AbstractClosure env x e) v a
    | v /= AbstractTop
    = Analysis.lookup e (Environment.insert x v env) a
    | otherwise
    = AbstractTop
refineApply AbstractTop _ _ = AbstractTop
refineApply _ _ _ = error "Cannot refine an abstract non-function"

refineEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractValue
refineEval (Variable x)   env a = Environment.lookup x env
refineEval e@(Lambda x b) env a = AbstractClosure env' x b
    where
      env' = Environment.restrict (freeVariables e) env
refineEval (Application e1 e2) env a
    = refineApply (Analysis.lookup e1 env a) (Analysis.lookup e2 env a) a
refineEval (Cons e1 e2) env a
    | v1 /= AbstractTop && v2 /= AbstractTop
    = AbstractPair v1 v2
    | otherwise
    = AbstractTop
    where
      v1 = Analysis.lookup e1 env a
      v2 = Analysis.lookup e2 env a

expandApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractAnalysis
expandApply (AbstractClosure env x e) v a
    | v /= AbstractTop
    = Analysis.expand e (Environment.insert x v env) a
    | otherwise
    = Analysis.empty
expandApply AbstractTop _ _ = Analysis.empty
expandApply _ _ _ = error "Cannot expand an abstract non-function"

expandEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractAnalysis
expandEval (Variable _) _ _ = Analysis.empty
expandEval (Lambda _ _) _ _ = Analysis.empty
expandEval (Application e1 e2) env a
    = Analysis.unions [ Analysis.expand e1 env a
                      , Analysis.expand e2 env a
                      , expandApply (Analysis.lookup e1 env a) (Analysis.lookup e2 env a) a
                      ]
expandEval (Cons e1 e2) env a
    = (Analysis.expand e1 env a) `Analysis.union` (Analysis.expand e2 env a)

u :: AbstractAnalysis -> AbstractAnalysis
u a = Analysis.unions . map u1 . Analysis.domain $ a
    where
      u1 (e, env) = Analysis.insert e env (refineEval e env a) (expandEval e env a)

-- NOTE: May not terminate
analyze :: CoreExpression
        -> Environment Scalar   -- Bindings produced by constant conversion
        -> AbstractAnalysis
analyze e constants = leastFixedPoint u a0
    where
      a0 = Analysis.singleton e (initialAbstractEnvironment ++ bindings) AbstractTop
      bindings = [(x, AbstractScalar v) | (x, v) <- constants]

leastFixedPoint :: Eq a => (a -> a) -> a -> a
leastFixedPoint f x | f x == x  = x
                    | otherwise = leastFixedPoint f (f x)

initialAbstractEnvironment :: AbstractEnvironment
initialAbstractEnvironment = [] -- will contain bindings for primitives eventually