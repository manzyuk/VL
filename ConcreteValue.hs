module VL.ConcreteValue where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment

data ConcreteValue
    = ConcreteScalar Scalar
    | ConcreteClosure ConcreteEnvironment Name CoreExpr
    | ConcretePair ConcreteValue ConcreteValue
      deriving (Eq, Ord, Show)

type ConcreteEnvironment = Environment ConcreteValue
