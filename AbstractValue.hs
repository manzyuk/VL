module VL.AbstractValue where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment (Environment, fromList)
import qualified VL.Environment as Environment

data AbstractValue
    = AbstractScalar Scalar
    | AbstractBoolean
    | AbstractReal
    | AbstractClosure AbstractEnvironment Name CoreExpression
    | AbstractPair AbstractValue AbstractValue
    | AbstractBottom
      deriving (Eq, Ord)

type AbstractEnvironment = Environment AbstractValue

-- Join in the join-semilattice of abstract values
joinValues :: AbstractValue -> AbstractValue -> AbstractValue
joinValues v1 v2
    | v1 == AbstractBottom
    = v2
    | v2 == AbstractBottom
    = v1
    | v1 == v2
    = v1
    | isSomeBoolean v1 && isSomeBoolean v2
    = AbstractBoolean
    | isSomeReal v1 && isSomeReal v2
    = AbstractReal
joinValues (AbstractClosure env1 x1 e1) (AbstractClosure env2 x2 e2)
    | x1 == x2 && e1 == e2
    = AbstractClosure (env1 `joinEnvironments` env2) x1 e1
    where
      -- The environment of a closure is assumed to contain precisely
      -- the free variables of the body, therefore if the bodies are
      -- equal, then the environments must contain the same set of
      -- variables.
      joinEnvironments env1 env2
          = fromList $ map joinBindings (Environment.domain env1)
      joinBindings x
          = (x, (Environment.lookup x env1) `joinValues` (Environment.lookup x env2))
joinValues (AbstractPair v1 v2) (AbstractPair v1' v2')
    = AbstractPair (v1 `joinValues` v1') (v2 `joinValues` v2')
joinValues _ _
    = error "joinValues: join doesn't exist"

isSomeBoolean, isSomeReal :: AbstractValue -> Bool

isSomeBoolean (AbstractScalar (Boolean _)) = True
isSomeBoolean AbstractBoolean              = True
isSomeBoolean _                            = False

isSomeReal    (AbstractScalar (Real _))    = True
isSomeReal    AbstractReal                 = True
isSomeReal    _                            = False