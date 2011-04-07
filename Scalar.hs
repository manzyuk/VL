module VL.Scalar where

import VL.Common
import VL.Environment (Environment)
import qualified VL.Environment as Environment

import Control.Arrow (second)

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
    | Exp | Log | Pow | Sin | Cos | Tan | Neg | Sqrt
    | Asin | Acos | Atan
    | Sinh | Cosh | Tanh
    | Asinh | Acosh | Atanh
    | IfProc
    | IsNull
    | IsPair
    | IsReal
    | IsBoolean
    | RealPrim
      deriving (Eq, Ord, Show)

type ScalarEnvironment = Environment Scalar

primitives :: ScalarEnvironment
primitives = Environment.fromList . map (second Primitive) $
             [ ("car"   , Car   )
             , ("cdr"   , Cdr   )
             , ("+"     , Add   )
             , ("-"     , Sub   )
             , ("*"     , Mul   )
             , ("/"     , Div   )
             , ("=="    , Eql   )
             , ("/="    , Neq   )
             , ("<"     , LTh   )
             , ("<="    , LEq   )
             , (">"     , GTh   )
             , (">="    , GEq   )
             , ("exp"   , Exp   )
             , ("log"   , Log   )
             , ("**"    , Pow   )
             , ("sin"   , Sin   )
             , ("cos"   , Cos   )
             , ("tan"   , Tan   )
             , ("sqrt"  , Sqrt  )
             , ("asin"  , Asin  )
             , ("acos"  , Acos  )
             , ("atan"  , Atan  )
             , ("sinh"  , Sinh  )
             , ("cosh"  , Cosh  )
             , ("tanh"  , Tanh  )
             , ("asinh" , Asinh )
             , ("acosh" , Acosh )
             , ("atanh" , Atanh )
             , ("negate", Neg   )
             , ("#:if-procedure", IfProc)
             , ("null?" , IsNull)
             , ("pair?" , IsPair)
             , ("real?" , IsReal)
             , ("boolean?", IsBoolean)
             , ("real"  , RealPrim)
             ]
