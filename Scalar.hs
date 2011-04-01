module VL.Scalar where

import VL.Common
import VL.Environment

data Scalar
    = Nil
    | Boolean Bool
    | Real Float
    | Primitive Name
      deriving (Eq, Ord, Show)

type ScalarEnvironment = Environment Scalar
