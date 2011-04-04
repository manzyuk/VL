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
    = Car | Cdr
    | Add | Sub | Mul | Div
    | Eql | Neq | LTh | LEq | GTh | GEq
    | Exp | Log | Pow | Sin | Cos | Tan | Sqrt
    | Asin | Acos | Atan
    | Sinh | Cosh | Tanh
    | Asinh | Acosh | Atanh
      deriving (Eq, Ord, Show)

type ScalarEnvironment = Environment Scalar
