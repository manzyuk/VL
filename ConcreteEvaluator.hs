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
dispatch Car = primCar
dispatch Cdr = primCdr
dispatch Add = arithmetic (+)
dispatch Sub = arithmetic (-)
dispatch Mul = arithmetic (*)
dispatch Div = arithmetic (/)

primCar :: ConcreteValue -> ConcreteValue
primCar (ConcretePair v1 _) = v1
primCar _ = error "Cannot apply car to a non-pair"

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr _ = error "Cannot apply cdr to a non-pair"

arithmetic :: (Float -> Float -> Float) -> ConcreteValue -> ConcreteValue
arithmetic op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Real (r1 `op` r2))
arithmetic _ _ = error "Cannot perform arithmetics on non-numbers"

primitives :: Environment Scalar
primitives = Environment.fromList . map (second Primitive) $
             [ ("car", Car)
             , ("cdr", Cdr)
             , ("+"  , Add)
             , ("-"  , Sub)
             , ("*"  , Mul)
             , ("/"  , Div)
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
