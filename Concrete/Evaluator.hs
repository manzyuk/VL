module VL.Concrete.Evaluator where

import VL.Language.Scalar
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Language.Parser
import VL.Language.Pretty
import VL.Language.Prepare

import VL.Concrete.Value

import Control.Monad
import System.IO
import Control.Exception

eval :: CoreExpr -> ConcreteEnvironment -> ConcreteValue
eval (Var x) env
    = Environment.lookup x env
eval e@(Lam formal body) env
    = ConcreteClosure env' formal body
    where
      env' = Environment.restrict (freeVariables e) env
eval (App operator operand) env
    = apply (eval operator env) (eval operand env)
eval (Pair car cdr) env
    = ConcretePair (eval car env) (eval cdr env)
eval (Letrec bindings body) env
    = eval (pushLetrec bindings body) env

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (Environment.insert x v env)
apply (ConcreteScalar (Primitive p)) v = dispatch p v
apply v _ = error $ "apply: can't apply " ++ show v

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

dispatch IsNull    = predicate isNull
dispatch IsPair    = predicate isPair
dispatch IsReal    = predicate isReal
dispatch IsBoolean = predicate isBoolean

dispatch RealPrim  = primReal

primCar :: ConcreteValue -> ConcreteValue
primCar (ConcretePair v1 _) = v1
primCar v = error $ "primCar: the argument is not a pair: " ++ show v

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr v = error $ "primCdr: the argument is not a pair: " ++ show v

unary :: (Float -> Float) -> ConcreteValue -> ConcreteValue
unary f (ConcreteScalar (Real r)) = ConcreteScalar (Real (f r))
unary _ v = error $ "unary: the argument is not a real: " ++ show v

arithmetic :: (Float -> Float -> Float) -> ConcreteValue -> ConcreteValue
arithmetic op (ConcretePair (ConcreteScalar (Real r1))
			    (ConcreteScalar (Real r2)))
    = ConcreteScalar (Real (r1 `op` r2))
arithmetic _ v
    = error $ "arithmetic: the argument is not a pair of reals: " ++ show v

comparison :: (Float -> Float -> Bool) -> ConcreteValue -> ConcreteValue
comparison op (ConcretePair (ConcreteScalar (Real r1))
			    (ConcreteScalar (Real r2)))
    = ConcreteScalar (Boolean (r1 `op` r2))
comparison _ v
    = error $ "comparison: the argument is not a pair of reals: " ++ show v

primIfProc :: ConcreteValue -> ConcreteValue
primIfProc (ConcretePair (ConcreteScalar (Boolean c))
			 (ConcretePair t e))
    | c
    = force t
    | otherwise
    = force e
    where
      force thunk = apply thunk (ConcreteScalar Nil)
primIfProc v = error $ "primIfProc: malformed IF expression: " ++ show v

predicate :: (ConcreteValue -> Bool) -> ConcreteValue -> ConcreteValue
predicate p = ConcreteScalar . Boolean . p

isNull, isPair, isReal, isBoolean :: ConcreteValue -> Bool
isNull (ConcreteScalar Nil)            = True
isNull _                               = False
isPair (ConcretePair _ _)              = True
isPair _                               = False
isReal (ConcreteScalar (Real _))       = True
isReal _                               = False
isBoolean (ConcreteScalar (Boolean _)) = True
isBoolean _                            = False

primReal :: ConcreteValue -> ConcreteValue
primReal v@(ConcreteScalar (Real _)) = v
primReal v = error $ "primReal: the argument is not a real: " ++ show v

interpret :: String -> String
interpret input = pprint $ eval (prepare expression) environment
    where
      (expression, constants) = parse input
      environment = Environment.map ConcreteScalar
		  $ primitives `Environment.union` constants

interpreter :: IO ()
interpreter = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever . handle (\e -> print (e :: ErrorCall)) $ repl
    where
      repl = do
	putStr prompt
	input <- getLine
	putStrLn $ interpret input

      prompt = "vl> "
