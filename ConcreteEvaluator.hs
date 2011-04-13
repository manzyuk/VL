{-# LANGUAGE TypeOperators #-}
module VL.ConcreteEvaluator where

import VL.Scalar
import VL.Coproduct
import VL.Expression
import VL.Macroexpand

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.ConcreteValue

import VL.Parser (parse)
import VL.Pretty (pprint)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad (forever)
import System.IO
import Control.Exception

-- The following meta-circular evaluator is not fully modular.  A more
-- general class @EvalExpr@ would have a method @evalExpr@ of the type
-- @(EvalExpr g) => f (Expr g) -> Env g -> Value g@, where @Env g@ and
-- @Value g@ are environment and value types parametrized by the
-- signature @g@ of the expression type.
class EvalCoreExpr f where
    evalCoreExpr :: f CoreExpression
                 -> ConcreteEnvironment
                 -> ConcreteValue

eval :: CoreExpression -> ConcreteEnvironment -> ConcreteValue
eval (In t) = evalCoreExpr t

instance EvalCoreExpr Variable where
    evalCoreExpr (Variable x) env = Environment.lookup x env

instance EvalCoreExpr LambdaOneArg where
    evalCoreExpr (LambdaOneArg arg body) env
        = ConcreteClosure env' arg body
        where
          fvs  = Set.delete arg (freeVariables body)
          env' = Environment.restrict fvs env

instance EvalCoreExpr ApplicationOneArg where
    evalCoreExpr (ApplicationOneArg operator operand) env
        = apply (eval operator env) (eval operand env)

instance EvalCoreExpr Cons where
    evalCoreExpr (Cons e1 e2) env
        = ConcretePair (eval e1 env) (eval e2 env)

instance EvalCoreExpr LetrecOneArg where
    evalCoreExpr (LetrecOneArg bindings body) env
        = eval (pushLetrec bindings body) env

instance (EvalCoreExpr f, EvalCoreExpr g) => EvalCoreExpr (f :+: g) where
    evalCoreExpr (Inl x) = evalCoreExpr x
    evalCoreExpr (Inr x) = evalCoreExpr x

apply :: ConcreteValue -> ConcreteValue -> ConcreteValue
apply (ConcreteClosure env x e) v = eval e (Environment.insert x v env)
apply (ConcreteScalar (Primitive p)) v = dispatch p v
apply _ _ = error "apply: can't apply a concrete non-function"

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
primCar _ = error "primCar: can't apply car to a non-pair"

primCdr :: ConcreteValue -> ConcreteValue
primCdr (ConcretePair _ v2) = v2
primCdr _ = error "primCdr: can't apply cdr to a non-pair"

unary :: (Float -> Float) -> ConcreteValue -> ConcreteValue
unary f (ConcreteScalar (Real r)) = ConcreteScalar (Real (f r))
unary _ _ = error "unary: can't perform arithmetics on non-numbers"

arithmetic :: (Float -> Float -> Float) -> ConcreteValue -> ConcreteValue
arithmetic op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Real (r1 `op` r2))
arithmetic _ _ = error "arithmetic: can't perform arithmetics on non-numbers"

comparison :: (Float -> Float -> Bool) -> ConcreteValue -> ConcreteValue
comparison op (ConcretePair (ConcreteScalar (Real r1))
                            (ConcreteScalar (Real r2)))
    = ConcreteScalar (Boolean (r1 `op` r2))
comparison _ _ = error "comparison: can't compare non-numbers"

primIfProc :: ConcreteValue -> ConcreteValue
primIfProc (ConcretePair (ConcreteScalar (Boolean c))
                         (ConcretePair t e))
    | c
    = force t
    | otherwise
    = force e
    where
      force thunk = apply thunk (ConcreteScalar Nil)
primIfProc v = error $ "Malformed IF expression: " ++ (pprint v)

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
primReal _ = error "primReal: the argument is not a real"

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
