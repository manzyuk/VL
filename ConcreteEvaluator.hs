module VL.ConcreteEvaluator where

import VL.Scalar
import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.ConcreteValue

import VL.Parser
import VL.Pretty

import Control.Arrow (second)
import Control.Monad (forever)
import System.IO

eval :: CoreExpression -> ConcreteEnvironment -> ConcreteValue
eval (Variable x)        env = Environment.lookup x env
eval e@(Lambda x b)      env = ConcreteClosure env' x b
    where
      env' = Environment.restrict (freeVariables e) env
eval (Application e1 e2) env = apply (eval e1 env) (eval e2 env)
eval (Cons e1 e2)        env = ConcretePair (eval e1 env) (eval e2 env)

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (Environment.insert x v env)
apply (ConcreteScalar (Primitive p)) v = dispatch p v
apply _ _ = error "Cannot apply a concrete non-function"

dispatch :: Primitive -> ConcreteValue -> ConcreteValue
dispatch Car   = primCar
dispatch Cdr   = primCdr
dispatch Add   = binary (+)
dispatch Sub   = binary (-)
dispatch Mul   = binary (*)
dispatch Div   = binary (/)
dispatch Pow   = binary (**)
dispatch Eql   = comparison (==)
dispatch Neq   = comparison (/=)
dispatch LTh   = comparison (<)
dispatch LEq   = comparison (<=)
dispatch GTh   = comparison (>)
dispatch GEq   = comparison (>=)
dispatch Exp   = unary exp
dispatch Log   = unary log
dispatch Sin   = unary sin
dispatch Cos   = unary cos
dispatch Tan   = unary tan
dispatch Sqrt  = unary sqrt
dispatch Asin  = unary asin
dispatch Acos  = unary acos
dispatch Atan  = unary atan
dispatch Sinh  = unary sinh
dispatch Cosh  = unary cosh
dispatch Tanh  = unary tanh
dispatch Asinh = unary asinh
dispatch Acosh = unary acosh
dispatch Atanh = unary atanh

primCar :: ConcreteValue -> ConcreteValue
primCar (ConcretePair v1 _) = v1
primCar _ = error "Cannot apply car to a non-pair"

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr _ = error "Cannot apply cdr to a non-pair"

unary :: (Float -> Float) -> ConcreteValue -> ConcreteValue
unary f (ConcreteScalar (Real r)) = ConcreteScalar (Real (f r))
unary _ _ = error "Cannot perform arithmetics on non-numbers"

binary :: (Float -> Float -> Float) -> ConcreteValue -> ConcreteValue
binary op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Real (r1 `op` r2))
binary _ _ = error "Cannot perform arithmetics on non-numbers"

comparison :: (Float -> Float -> Bool) -> ConcreteValue -> ConcreteValue
comparison op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Boolean (r1 `op` r2))
comparison _ _ = error "Cannot compare non-numbers"

primitives :: Environment Scalar
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
             ]

interpreter :: IO ()
interpreter = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever repl
    where
      repl = do
        putStr prompt
        input <- getLine
        let (expression, constants) = parse input
            environment = Environment.map ConcreteScalar $
                          primitives `Environment.union` constants
        putStrLn . render . pp $ eval expression environment

      prompt = "vl> "
