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
-- The evaluation rule for `letrec' is taken from "Theories of
-- Programming Languages" by John C. Reynolds (Section 11.3, p. 230):
--
-- letrec v1 = \u1. e1, ..., vn = \un. en in e
--   == (\v1. ... \vn. e) (\u1. e1*) ... (\un. en*)
--
-- where ei* = letrec v1 = \u1. e1, ..., vn = \un. en in ei.
eval (Letrec ls b)       env = foldl apply f fs
    where
      ns = [n | (n, _, _) <- ls]
      f  = eval (foldr Lambda b ns) env
      fs = [eval (Lambda x (Letrec ls e)) env | (_, x, e) <- ls]

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (Environment.insert x v env)
apply (ConcreteScalar (Primitive p)) v = dispatch p v
apply _ _ = error "Cannot apply a concrete non-function"

dispatch :: Primitive -> ConcreteValue -> ConcreteValue
dispatch Car   = primCar
dispatch Cdr   = primCdr
dispatch Add   = arithmetic (+)
dispatch Sub   = arithmetic (-)
dispatch Mul   = arithmetic (*)
dispatch Div   = arithmetic (/)
dispatch Pow   = arithmetic (**)
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
dispatch Neg   = unary negate

dispatch IfProc = primIfProc

primCar :: ConcreteValue -> ConcreteValue
primCar (ConcretePair v1 _) = v1
primCar _ = error "Cannot apply car to a non-pair"

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr _ = error "Cannot apply cdr to a non-pair"

unary :: (Float -> Float) -> ConcreteValue -> ConcreteValue
unary f (ConcreteScalar (Real r)) = ConcreteScalar (Real (f r))
unary _ _ = error "Cannot perform arithmetics on non-numbers"

arithmetic :: (Float -> Float -> Float) -> ConcreteValue -> ConcreteValue
arithmetic op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Real (r1 `op` r2))
arithmetic _ _ = error "Cannot perform arithmetics on non-numbers"

comparison :: (Float -> Float -> Bool) -> ConcreteValue -> ConcreteValue
comparison op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Boolean (r1 `op` r2))
comparison _ _ = error "Cannot compare non-numbers"

primIfProc :: ConcreteValue -> ConcreteValue
primIfProc (ConcretePair (ConcreteScalar (Boolean c))
                         (ConcretePair t e))
    | c
    = force t
    | otherwise
    = force e
    where
      force thunk = apply thunk (ConcreteScalar Nil)

interpret :: String -> String
interpret input = render . pp $ eval expression environment
    where
      (expression, constants) = parse input
      environment = Environment.map ConcreteScalar
                  $ primitives `Environment.union` constants

interpreter :: IO ()
interpreter = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever repl
    where
      repl = do
        putStr prompt
        input <- getLine
        putStrLn $ interpret input

      prompt = "vl> "
