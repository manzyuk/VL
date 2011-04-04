module VL.ConcreteEvaluator where

import VL.Scalar
import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.ConcreteValue

import VL.Parser
import VL.Pretty

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

primCar :: ConcreteValue -> ConcreteValue
primCar (ConcretePair v1 _) = v1
primCar _ = error "Cannot apply car to a non-pair"

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr _ = error "Cannot apply cdr to a non-pair"

primitives :: Environment Scalar
primitives = Environment.fromList
             [ ("car", Primitive Car)
             , ("cdr", Primitive Cdr)
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
