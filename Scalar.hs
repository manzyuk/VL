module VL.Scalar where

import VL.Common
import VL.Environment (Environment)
import qualified VL.Environment as Environment

import Control.Arrow

data Scalar
    = Nil
    | Boolean Bool
    | Real Float
    | Primitive Primitive
      deriving (Eq, Ord, Show)

data Primitive
    = Car | Cdr
    -- arithmetic
    | Add | Sub | Mul | Div | Neg
    -- comparison
    | Eql | Neq | LTh | LEq | GTh | GEq
    -- exp and friends
    | Exp | Log | Pow | Sqrt
    -- trigonometric functions
    | Sin   | Cos   | Tan
    | Asin  | Acos  | Atan
    | Sinh  | Cosh  | Tanh
    | Asinh | Acosh | Atanh
    -- IF-PROCEDURE primitive
    | IfProc
    -- shape predicates
    | IsNull
    | IsPair
    | IsReal
    | IsBoolean
    -- REAL primitive
    | RealPrim
      deriving (Eq, Ord, Show)

type ScalarEnvironment = Environment Scalar

primitives :: ScalarEnvironment
primitives = Environment.fromList . map (second Primitive) $
             [ ("car"            , Car       )
             , ("cdr"            , Cdr       )

             , ("+"              , Add       )
             , ("-"              , Sub       )
             , ("*"              , Mul       )
             , ("/"              , Div       )
             , ("negate"         , Neg       )

             , ("=="             , Eql       )
             , ("/="             , Neq       )
             , ("<"              , LTh       )
             , ("<="             , LEq       )
             , (">"              , GTh       )
             , (">="             , GEq       )

             , ("exp"            , Exp       )
             , ("log"            , Log       )
             , ("**"             , Pow       )
             , ("sqrt"           , Sqrt      )

             , ("sin"            , Sin       )
             , ("cos"            , Cos       )
             , ("tan"            , Tan       )
             , ("asin"           , Asin      )
             , ("acos"           , Acos      )
             , ("atan"           , Atan      )
             , ("sinh"           , Sinh      )
             , ("cosh"           , Cosh      )
             , ("tanh"           , Tanh      )
             , ("asinh"          , Asinh     )
             , ("acosh"          , Acosh     )
             , ("atanh"          , Atanh     )

             , ("#:if-procedure" , IfProc    )

             , ("null?"          , IsNull    )
             , ("pair?"          , IsPair    )
             , ("real?"          , IsReal    )
             , ("boolean?"       , IsBoolean )

             , ("real"           , RealPrim  )
             ]
