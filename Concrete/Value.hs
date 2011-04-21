module VL.Concrete.Value where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Pretty
import VL.Language.Expression
import VL.Language.Environment

data ConcreteValue
    = ConcreteScalar Scalar
    | ConcreteClosure ConcreteEnvironment Name CoreExpr
    | ConcretePair ConcreteValue ConcreteValue
      deriving (Eq, Ord, Show)

type ConcreteEnvironment = Environment ConcreteValue

-- Pretty-printing of concrete values
instance Pretty ConcreteValue where
    pp (ConcreteScalar s)        = pp s
    pp (ConcreteClosure env x b) = ppClosure env x b
    pp (ConcretePair v1 v2)      = ppPair v1 v2
