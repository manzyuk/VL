module VL.Abstract.Value where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Pretty
import VL.Language.Expression
import VL.Language.Environment (Environment)
import qualified VL.Language.Environment as Environment

data AbstractValue
    = AbstractScalar Scalar
    | AbstractBoolean
    | AbstractReal
    | AbstractClosure AbstractEnvironment Name CoreExpr
    | AbstractPair AbstractValue AbstractValue
    | AbstractBottom
      deriving (Eq, Ord, Show)

type AbstractEnvironment = Environment AbstractValue

-- The set of abstract values is a partial order (in fact, a cpo):
--   * AbstractBottom is the least element;
--   * AbstractReal is greater than any concrete real;
--   * AbstractBoolean is greater than any concrete boolean;
--   * for abstract pairs,
--       (x1, y1) >= (x2, y2) iff x1 >= x2 and y1 >= y2;
--   * for abstract closures,
--                                         exp1 == exp2
--       (env1, exp1) >= (env2, exp2) iff      and
--                                         env1 >= env2;
--     here env1 >= env2 iff env1 and env2 contain the same variables
--     (which is the case if exp1 == exp2, by our assumption about
--     closures) and for every variable x, env1 x >= env2 x.
-- The function 'joinValues' implements join in the poset of abstract
-- values.  WARNING: Not every pair of abstract values has a join!
joinValues :: AbstractValue -> AbstractValue -> AbstractValue
joinValues AbstractBottom v2 = v2
joinValues v1 AbstractBottom = v1
joinValues v1 v2
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
      joinEnvironments env _
          = Environment.fromList $ map joinBindings (Environment.domain env)
      joinBindings x
          = (x, (Environment.lookup x env1) `joinValues` (Environment.lookup x env2))
joinValues (AbstractPair v1 v2) (AbstractPair v1' v2')
    = AbstractPair (v1 `joinValues` v1') (v2 `joinValues` v2')
joinValues v1 v2
    = error $ unwords [ "joinValues: join of"
                      , show v1
                      , "and"
                      , show v2
                      , "doesn't exist"
                      ]

isSomeBoolean, isSomeReal :: AbstractValue -> Bool

isSomeBoolean (AbstractScalar (Boolean _)) = True
isSomeBoolean AbstractBoolean              = True
isSomeBoolean _                            = False

isSomeReal    (AbstractScalar (Real _))    = True
isSomeReal    AbstractReal                 = True
isSomeReal    _                            = False

-- Pretty-printing of abstract values
instance Pretty AbstractValue where
    pp (AbstractScalar s)        = pp s
    pp AbstractBoolean           = text "B"
    pp AbstractReal              = text "R"
    pp AbstractBottom            = text "_|_"
    pp (AbstractClosure env x b) = ppClosure env x b
    pp (AbstractPair v1 v2)      = ppPair v1 v2
