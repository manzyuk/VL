module VL.Concrete.Value where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Expression
import VL.Language.Environment

data ConcreteValue
    = ConcreteScalar Scalar
    | ConcreteClosure ConcreteEnvironment Name CoreExpr
    | ConcretePair ConcreteValue ConcreteValue
      deriving (Eq, Ord, Show)

type ConcreteEnvironment = Environment ConcreteValue
