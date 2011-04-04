module VL.Scalar where

import VL.Common
import VL.Environment

data Scalar
    = Nil
    | Boolean Bool
    | Real Float
    | Primitive Primitive
      deriving (Eq, Ord, Show)

data Primitive
    = Car
    | Cdr
      deriving (Eq, Ord, Show)

type ScalarEnvironment = Environment Scalar
