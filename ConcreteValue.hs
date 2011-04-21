module VL.ConcreteValue where

import VL.Common
import VL.Scalar
import VL.Syntax
import VL.Environment

data ConcreteValue
    = ConcreteScalar Scalar
    | ConcreteClosure ConcreteEnvironment Name CoreExpression
    | ConcretePair ConcreteValue ConcreteValue
      deriving (Eq, Ord)

type ConcreteEnvironment = Environment ConcreteValue
