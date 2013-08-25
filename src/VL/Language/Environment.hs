module VL.Language.Environment
    ( Environment
    , map
    , empty
    , union
    , domain
    , values
    , lookup
    , insert
    , update
    , mapKeys
    , bindings
    , restrict
    , fromList
    , singleton
    )
    where

import VL.Language.Common
import VL.Language.Pretty hiding (empty)

import Prelude hiding (map, lookup)
import qualified Data.List as List

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow

newtype Environment val
    = Environment { bindings :: [(Name, val)] } deriving (Eq, Ord, Show)

empty :: Environment val
empty = Environment []

union :: Eq val => Environment val -> Environment val -> Environment val
union env1 env2 = Environment (bindings env1 `List.union` bindings env2)

domain :: Environment val -> [Name]
domain env = [x | (x, _) <- bindings env]

values :: Environment val -> [val]
values env = [v | (_, v) <- bindings env]

lookup :: Name -> Environment val -> val
lookup x env
    = fromMaybe (error msg) (List.lookup x (bindings env))
      where
        msg = "Unbound variable: " ++ x

insert :: Name -> val -> Environment val -> Environment val
insert x v env = Environment ((x, v) : bindings env)

update :: Name -> val -> Environment val -> Environment val
update x v env = maybe (insert x v env) (const env) (List.lookup x (bindings env))

restrict :: Set Name -> Environment val -> Environment val
restrict set env = Environment [ (x, v)
                               | (x, v) <- bindings env
                               , x `Set.member` set
                               ]

fromList :: [(Name, val)] -> Environment val
fromList = Environment

singleton :: Name -> val -> Environment val
singleton x v = Environment [(x, v)]

map :: (val1 -> val2) -> Environment val1 -> Environment val2
map f = Environment . List.map (second f) . bindings

mapKeys :: (Name -> Name) -> Environment val -> Environment val
mapKeys f = Environment . List.map (first f) . bindings

-- Pretty-printing of environments
instance Pretty val => Pretty (Environment val) where
    pp = ppList . List.map ppBinding . bindings
        where
          ppBinding (x, v) = ppPair x v
