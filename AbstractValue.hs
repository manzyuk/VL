module VL.AbstractValue where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment (Environment)
import qualified VL.Environment as Environment

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
          = map unifyBindings (Environment.domain env1)
      unifyBindings x
          = (x, (Environment.lookup x env1) `unifyValues` (Environment.lookup x env2))
unifyValues (AbstractPair v1 v2) (AbstractPair v1' v2')
    = AbstractPair (v1 `unifyValues` v1') (v2 `unifyValues` v2')
unifyValues _ _
    = AbstractTop
