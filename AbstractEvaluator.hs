{-# LANGUAGE TypeOperators #-}
module VL.AbstractEvaluator where

import VL.Common
import VL.Scalar
import VL.Coproduct
import VL.Expression
import VL.Macroexpand

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.AbstractValue

import VL.AbstractAnalysis (AbstractAnalysis)
import qualified VL.AbstractAnalysis as Analysis

import VL.Parser (parse)
import VL.Pretty

import Prelude hiding (read)

import Data.Maybe (isJust)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow ((***))

import Control.Monad (forever)
import System.IO

refineApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractValue
refineApply (AbstractClosure env x e) v a
    | v /= AbstractBottom
    = Analysis.lookup e (Environment.insert x v env) a
    | otherwise
    = AbstractBottom
refineApply (AbstractScalar (Primitive p)) v a
    = refinePrimitive p a v
refineApply AbstractBottom _ _ = AbstractBottom
refineApply _ _ _ = error "refineApply: can't refine an abstract non-function"

refinePrimitive :: Primitive
                -> AbstractAnalysis
                -> AbstractValue
                -> AbstractValue
refinePrimitive Car   _ = primCar
refinePrimitive Cdr   _ = primCdr
refinePrimitive Add   _ = arithmetic (+)
refinePrimitive Sub   _ = arithmetic (-)
refinePrimitive Mul   _ = arithmetic (*)
refinePrimitive Div   _ = arithmetic (/)
refinePrimitive Pow   _ = arithmetic (**)
refinePrimitive Eql   _ = comparison (==)
refinePrimitive Neq   _ = comparison (/=)
refinePrimitive LTh   _ = comparison (<)
refinePrimitive LEq   _ = comparison (<=)
refinePrimitive GTh   _ = comparison (>)
refinePrimitive GEq   _ = comparison (>=)
refinePrimitive Exp   _ = unary exp
refinePrimitive Log   _ = unary log
refinePrimitive Sin   _ = unary sin
refinePrimitive Cos   _ = unary cos
refinePrimitive Tan   _ = unary tan
refinePrimitive Sqrt  _ = unary sqrt
refinePrimitive Asin  _ = unary asin
refinePrimitive Acos  _ = unary acos
refinePrimitive Atan  _ = unary atan
refinePrimitive Sinh  _ = unary sinh
refinePrimitive Cosh  _ = unary cosh
refinePrimitive Tanh  _ = unary tanh
refinePrimitive Asinh _ = unary asinh
refinePrimitive Acosh _ = unary acosh
refinePrimitive Atanh _ = unary atanh
refinePrimitive Neg   _ = unary negate

refinePrimitive IfProc a = refineIfProc a

refinePrimitive IsNull _    = predicate isNull
refinePrimitive IsPair _    = predicate isPair
refinePrimitive IsReal _    = predicate isReal
refinePrimitive IsBoolean _ = predicate isBoolean

refinePrimitive RealPrim _  = primReal

primCar :: AbstractValue -> AbstractValue
primCar (AbstractPair v1 _) = v1
primCar AbstractBottom = AbstractBottom
primCar _ = error "primCar: provably a non-pair where a pair is expected"

primCdr :: AbstractValue -> AbstractValue
primCdr (AbstractPair _ v2) = v2
primCdr AbstractBottom = AbstractBottom
primCdr _ = error "primCdr: provably a non-pair where a pair is expected"

dyadic :: (AbstractValue -> AbstractValue -> AbstractValue)
       -> AbstractValue
       -> AbstractValue
dyadic op (AbstractPair v1 v2) = v1 `op` v2
dyadic op AbstractBottom = AbstractBottom
dyadic op _ = error "dyadic: provably a non-pair where a pair is expected"

data Abstraction a
    = Abstraction {
      -- view a concrete value of type a as an abstract value
        convert  :: a -> AbstractValue
      -- try to extract a value of type a from an abstract value
      , extract  :: AbstractValue -> Maybe a
      -- abstract value corresponding to concrete values of type a
      , abstract :: AbstractValue
      }

float :: Abstraction Float
float = Abstraction {
          convert  = AbstractScalar . Real
        , extract  = maybeReal
        , abstract = AbstractReal
        }
    where
      maybeReal (AbstractScalar (Real r)) = Just r
      maybeReal _                         = Nothing

bool :: Abstraction Bool
bool = Abstraction {
         convert  = AbstractScalar . Boolean
       , extract  = maybeBoolean
       , abstract = AbstractBoolean
       }
    where
      maybeBoolean (AbstractScalar (Boolean b)) = Just b
      maybeBoolean _                            = Nothing

isSomeOfType :: Abstraction a -> AbstractValue -> Bool
isSomeOfType a v = isJust (extract a v) || v == abstract a

isNotSomeOfType :: Abstraction a -> AbstractValue -> Bool
isNotSomeOfType a = not . isSomeOfType a

liftOp :: Abstraction a -> Abstraction b -> Abstraction c
       -> (a -> b -> c)
       -> (AbstractValue -> AbstractValue -> AbstractValue)
liftOp a b c op x y
    | isNotSomeOfType a x || isNotSomeOfType b y
    = error "liftOp: type error"
    | x == abstract a || y == abstract b
    = abstract c
    | otherwise
    = convert c (u `op` v)
      where
        Just u = extract a x
        Just v = extract b y

arithmetic :: (Float -> Float -> Float) -> AbstractValue -> AbstractValue
arithmetic op = dyadic $ liftOp float float float op

comparison :: (Float -> Float -> Bool) -> AbstractValue -> AbstractValue
comparison op = dyadic $ liftOp float float bool op

unary :: (Float -> Float) -> AbstractValue -> AbstractValue
unary f (AbstractScalar (Real r)) = AbstractScalar (Real (f r))
unary _ AbstractReal = AbstractReal
unary _ AbstractBottom  = AbstractBottom
unary _ _ = error "unary: provably a non-number where a number is expected"

refineIfProc :: AbstractAnalysis -> AbstractValue -> AbstractValue
refineIfProc a (AbstractPair (AbstractScalar (Boolean c))
                             (AbstractPair t e))
    | c
    = refineThunk t a
    | otherwise
    = refineThunk e a
refineIfProc a (AbstractPair AbstractBoolean
                             (AbstractPair t e))
    = (refineThunk t a) `joinValues` (refineThunk e a)
refineIfProc a AbstractBottom
    = AbstractBottom
refineIfProc a _
    = error "refineIfProc: provably a non-boolean where a boolean is expected"

refineThunk :: AbstractValue -> AbstractAnalysis -> AbstractValue
refineThunk (AbstractClosure env x e) a
    -- We assume that x does not occur in e
    = refineEval e env a
refineThunk _ _ = error "refineThunk: the argument is not a thunk"

predicate :: (AbstractValue -> Bool) -> AbstractValue -> AbstractValue
predicate p = AbstractScalar . Boolean . p

isNull, isPair, isReal, isBoolean :: AbstractValue -> Bool
isNull (AbstractScalar Nil)            = True
isNull _                               = False
isPair (AbstractPair _ _)              = True
isPair _                               = False
isReal (AbstractScalar (Real _))       = True
isReal _                               = False
isBoolean (AbstractScalar (Boolean _)) = True
isBoolean _                            = False

primReal :: AbstractValue -> AbstractValue
primReal (AbstractScalar (Real _)) = AbstractReal
primReal AbstractReal              = AbstractReal
primReal AbstractBottom               = AbstractBottom
primReal _ = error "primReal: the argument is not an abstract real"

class RefineEvalCoreExpr f where
    refineEvalCoreExpr :: f CoreExpression
                       -> AbstractEnvironment
                       -> AbstractAnalysis
                       -> AbstractValue

refineEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractValue
refineEval (In t) = refineEvalCoreExpr t

instance RefineEvalCoreExpr Variable where
    refineEvalCoreExpr (Variable x) env a = Environment.lookup x env

instance RefineEvalCoreExpr LambdaOneArg where
    refineEvalCoreExpr (LambdaOneArg arg body) env a
        = AbstractClosure env' arg body
        where
          fvs  = Set.delete arg (freeVariables body)
          env' = Environment.restrict fvs env

instance RefineEvalCoreExpr ApplicationOneArg where
    refineEvalCoreExpr (ApplicationOneArg operator operand) env a
        = refineApply (Analysis.lookup operator env a)
                      (Analysis.lookup operand  env a) a

instance RefineEvalCoreExpr Cons where
    refineEvalCoreExpr (Cons e1 e2) env a
        | v1 /= AbstractBottom && v2 /= AbstractBottom
        = AbstractPair v1 v2
        | otherwise
        = AbstractBottom
        where
          v1 = Analysis.lookup e1 env a
          v2 = Analysis.lookup e2 env a

instance RefineEvalCoreExpr LetrecOneArg where
    refineEvalCoreExpr (LetrecOneArg bindings body) env a
        = refineEval (pushLetrec bindings body) env a

instance (RefineEvalCoreExpr f, RefineEvalCoreExpr g) =>
    RefineEvalCoreExpr (f :+: g) where
        refineEvalCoreExpr (Inl x) = refineEvalCoreExpr x
        refineEvalCoreExpr (Inr x) = refineEvalCoreExpr x

expandApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractAnalysis
expandApply (AbstractClosure env x e) v a
    | v /= AbstractBottom
    = Analysis.expand e (Environment.insert x v env) a
    | otherwise
    = Analysis.empty
expandApply (AbstractScalar (Primitive p)) v a
    = expandPrimitive p v a
expandApply AbstractBottom _ _ = Analysis.empty
expandApply _ _ _ = error "expandApply: can't expand an abstract non-function"

expandPrimitive :: Primitive
                -> AbstractValue
                -> AbstractAnalysis
                -> AbstractAnalysis
expandPrimitive IfProc v a = expandIfProc v a
expandPrimitive _ _ _      = Analysis.empty

expandIfProc :: AbstractValue -> AbstractAnalysis -> AbstractAnalysis
expandIfProc (AbstractPair (AbstractScalar (Boolean c))
                           (AbstractPair t e)) a
    | c
    = expandThunk t a
    | otherwise
    = expandThunk e a
expandIfProc (AbstractPair AbstractBoolean
                           (AbstractPair t e)) a
    = (expandThunk t a) `Analysis.union` (expandThunk e a)
expandIfProc AbstractBottom _
    = Analysis.empty
expandIfProc _ _
    = error "expandIfProc: provably a non-boolean where a boolean is expected"

expandThunk :: AbstractValue -> AbstractAnalysis -> AbstractAnalysis
expandThunk (AbstractClosure env x e) a
    -- We assume that x does not occur in e
    = Analysis.expand e env a
expandThunk _ _ = error "expandThunk: the argument is not a thunk"

class ExpandEvalCoreExpr f where
    expandEvalCoreExpr :: f CoreExpression
                       -> AbstractEnvironment
                       -> AbstractAnalysis
                       -> AbstractAnalysis

expandEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractAnalysis
expandEval (In t) = expandEvalCoreExpr t

instance ExpandEvalCoreExpr Variable where
    expandEvalCoreExpr (Variable _) _ _ = Analysis.empty

instance ExpandEvalCoreExpr LambdaOneArg where
    expandEvalCoreExpr (LambdaOneArg _ _) _ _ = Analysis.empty

instance ExpandEvalCoreExpr ApplicationOneArg where
    expandEvalCoreExpr (ApplicationOneArg operator operand) env a
        = Analysis.unions
          [ Analysis.expand operator env a
          , Analysis.expand operand  env a
          , expandApply (Analysis.lookup operator env a)
                        (Analysis.lookup operand  env a) a
          ]

instance ExpandEvalCoreExpr Cons where
    expandEvalCoreExpr (Cons e1 e2) env a
        = (Analysis.expand e1 env a) `Analysis.union` (Analysis.expand e2 env a)

instance ExpandEvalCoreExpr LetrecOneArg where
    expandEvalCoreExpr (LetrecOneArg bindings body) env a
        = expandEval (pushLetrec bindings body) env a

instance (ExpandEvalCoreExpr f, ExpandEvalCoreExpr g) =>
    ExpandEvalCoreExpr (f :+: g) where
        expandEvalCoreExpr (Inl x) = expandEvalCoreExpr x
        expandEvalCoreExpr (Inr x) = expandEvalCoreExpr x

amendAnalysis :: AbstractAnalysis -> AbstractAnalysis
amendAnalysis a = Analysis.unions . map amendBinding . Analysis.domain $ a
    where
      amendBinding (e, env)
          = Analysis.insert e env (refineEval e env a) (expandEval e env a)

-- NOTE: May not terminate
analyze :: (CoreExpression, ScalarEnvironment) -> AbstractAnalysis
analyze = last . analyze'

analyze' :: (CoreExpression, ScalarEnvironment) -> [AbstractAnalysis]
analyze' (expression, constants)
    = iterateUntilStable amendAnalysis analysis0
    where
      analysis0   = Analysis.singleton expression environment AbstractBottom
      environment = Environment.map AbstractScalar
                  $ primitives `Environment.union` constants

iterateUntilStable :: Eq a => (a -> a) -> a -> [a]
iterateUntilStable f x = (x:) . map snd . takeWhile (uncurry (/=)) $ zs
    where
      ys = iterate f x
      zs = zip ys (tail ys)

read :: String -> (CoreExpression, ScalarEnvironment)
read = (prepare *** id) . parse

interpret :: String -> String
interpret = render . pp . analyze . read

interpret' :: String -> String
interpret' = unlines . map (render . pp) . analyze' . read

verbose :: Bool
verbose = False

interpreter :: IO ()
interpreter = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever repl
    where
      repl = do
        putStr prompt
        input <- getLine
        putStrLn $ process input

      prompt = "vl> "
      process | verbose   = interpret'
              | otherwise = interpret
