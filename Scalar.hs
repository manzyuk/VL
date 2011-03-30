module VL.Scalar where

import VL.Common

data Scalar
    = Nil
    | Boolean Bool
    | Real Float
    | Primitive Name
      deriving (Eq, Ord, Show)
