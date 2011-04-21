module VL.Language.Scalar where

import VL.Language.Common
import VL.Language.Pretty
import VL.Language.Environment (Environment)
import qualified VL.Language.Environment as Environment

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

-- Pretty-printing of scalars
instance Pretty Scalar where
    pp Nil             = parens empty
    pp (Boolean True)  = text "#t"
    pp (Boolean False) = text "#f"
    pp (Real r)        = float r
    pp (Primitive p)   = pp p

-- Pretty-printing of primitives
instance Pretty Primitive where
    pp Car       = prim "car"
    pp Cdr       = prim "cdr"
    pp Add       = prim "+"
    pp Sub       = prim "-"
    pp Mul       = prim "*"
    pp Div       = prim "/"
    pp Eql       = prim "=="
    pp Neq       = prim "/="
    pp LTh       = prim "<"
    pp LEq       = prim "<="
    pp GTh       = prim ">"
    pp GEq       = prim ">="
    pp Exp       = prim "exp"
    pp Log       = prim "log"
    pp Pow       = prim "**"
    pp Sin       = prim "sin"
    pp Cos       = prim "cos"
    pp Tan       = prim "tan"
    pp Sqrt      = prim "sqrt"
    pp Asin      = prim "asin"
    pp Acos      = prim "acos"
    pp Atan      = prim "atan"
    pp Sinh      = prim "sinh"
    pp Cosh      = prim "cosh"
    pp Tanh      = prim "tanh"
    pp Asinh     = prim "asinh"
    pp Acosh     = prim "acosh"
    pp Atanh     = prim "atanh"
    pp Neg       = prim "negate"
    pp IfProc    = prim "if-procedure"
    pp IsNull    = prim "null?"
    pp IsPair    = prim "pair?"
    pp IsReal    = prim "real?"
    pp IsBoolean = prim "boolean?"
    pp RealPrim  = prim "real"

prim :: String -> Doc
prim = internal "primitive" . text
