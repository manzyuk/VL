module VL.AbstractEvaluator where

import VL.Common
import VL.Scalar
import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.AbstractValue

import VL.AbstractAnalysis (AbstractAnalysis)
import qualified VL.AbstractAnalysis as Analysis

import VL.Parser
import VL.Pretty

import Control.Monad (forever)
import System.IO

refineApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractValue
refineApply (AbstractClosure env x e) v a
    | v /= AbstractTop
    = Analysis.lookup e (Environment.insert x v env) a
    | otherwise
    = AbstractTop
refineApply (AbstractScalar (Primitive p)) v a
    = refinePrimitive p a v
refineApply AbstractTop _ _ = AbstractTop
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

primCar :: AbstractValue -> AbstractValue
primCar (AbstractPair v1 _) = v1
primCar AbstractTop = AbstractTop
primCar _ = error "primCar: provably a non-pair where a pair is expected"

primCdr :: AbstractValue -> AbstractValue
primCdr (AbstractPair _ v2) = v2
primCdr AbstractTop = AbstractTop
primCdr _ = error "primCdr: provably a non-pair where a pair is expected"

dyadic :: (AbstractValue -> AbstractValue -> AbstractValue)
       -> AbstractValue
       -> AbstractValue
dyadic op (AbstractPair v1 v2) = v1 `op` v2
dyadic op AbstractTop = AbstractTop
dyadic op _ = error "dyadic: provably a non-pair where a pair is expected"

liftOp :: (Float -> Float -> AbstractValue)
       -> AbstractValue
       -> (AbstractValue -> AbstractValue -> AbstractValue)
liftOp op c = l
    where
      l v1 v2
          | isNotSomeReal v1 || isNotSomeReal v2
          = error "listOp: provably a non-number where a number is expected"
          | v1 == AbstractReal || v2 == AbstractReal
          = c
      l (AbstractScalar (Real r1)) (AbstractScalar (Real r2)) = r1 `op` r2

isNotSomeReal :: AbstractValue -> Bool
isNotSomeReal = not . isSomeReal

isSomeReal :: AbstractValue -> Bool
isSomeReal (AbstractScalar (Real _)) = True
isSomeReal AbstractReal              = True
isSomeReal _                         = False

arithmetic :: (Float -> Float -> Float) -> AbstractValue -> AbstractValue
arithmetic op = dyadic $ liftOp op' AbstractReal
    where
      op' r1 r2 = AbstractScalar (Real (r1 `op` r2))

comparison :: (Float -> Float -> Bool) -> AbstractValue -> AbstractValue
comparison op = dyadic $ liftOp op' AbstractBoolean
    where
      op' r1 r2 = AbstractScalar (Boolean (r1 `op` r2))

unary :: (Float -> Float) -> AbstractValue -> AbstractValue
unary f (AbstractScalar (Real r)) = AbstractScalar (Real (f r))
unary _ AbstractReal = AbstractReal
unary _ AbstractTop  = AbstractTop
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
    = (refineThunk t a) `unifyValues` (refineThunk e a)
refineIfProc a AbstractTop
    = AbstractTop
refineIfProc a _
    = error "refineIfProc: provably a non-boolean where a boolean is expected"

refineThunk :: AbstractValue -> AbstractAnalysis -> AbstractValue
refineThunk (AbstractClosure env x e) a
    -- We assume that x does not occur in e
    = refineEval e env a
refineThunk _ _ = error "refineThunk: the argument is not a thunk"

refineEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractValue
refineEval (Variable x)   env a = Environment.lookup x env
refineEval e@(Lambda x b) env a = AbstractClosure env' x b
    where
      env' = Environment.restrict (freeVariables e) env
refineEval (Application e1 e2) env a
    = refineApply (Analysis.lookup e1 env a) (Analysis.lookup e2 env a) a
refineEval (Cons e1 e2) env a
    | v1 /= AbstractTop && v2 /= AbstractTop
    = AbstractPair v1 v2
    | otherwise
    = AbstractTop
    where
      v1 = Analysis.lookup e1 env a
      v2 = Analysis.lookup e2 env a

expandApply :: AbstractValue
            -> AbstractValue
            -> AbstractAnalysis
            -> AbstractAnalysis
expandApply (AbstractClosure env x e) v a
    | v /= AbstractTop
    = Analysis.expand e (Environment.insert x v env) a
    | otherwise
    = Analysis.empty
expandApply (AbstractScalar (Primitive p)) v a
    = expandPrimitive p v a
expandApply AbstractTop _ _ = Analysis.empty
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
expandIfProc AbstractTop _ = Analysis.empty
expandIfProc _ _ = error "expandIfProc: malformed `if'"

expandThunk :: AbstractValue -> AbstractAnalysis -> AbstractAnalysis
expandThunk (AbstractClosure env x e) a
    -- We assume that x does not occur in e
    = Analysis.expand e env a
expandThunk _ _ = error "expandThunk: the argument is not a thunk"

expandEval :: CoreExpression
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractAnalysis
expandEval (Variable _) _ _ = Analysis.empty
expandEval (Lambda _ _) _ _ = Analysis.empty
expandEval (Application e1 e2) env a
    = Analysis.unions
      [ Analysis.expand e1 env a
      , Analysis.expand e2 env a
      , expandApply (Analysis.lookup e1 env a) (Analysis.lookup e2 env a) a
      ]
expandEval (Cons e1 e2) env a
    = (Analysis.expand e1 env a) `Analysis.union` (Analysis.expand e2 env a)

amendAnalysis :: AbstractAnalysis -> AbstractAnalysis
amendAnalysis a = Analysis.unions . map amendBinding . Analysis.domain $ a
    where
      amendBinding (e, env)
          = Analysis.insert e env (refineEval e env a) (expandEval e env a)

-- NOTE: May not terminate
analyze :: (CoreExpression , ScalarEnvironment) -> AbstractAnalysis
analyze = last . analyze'

analyze' :: (CoreExpression , ScalarEnvironment) -> [AbstractAnalysis]
analyze' (expression, constants) = iterateUntilStable amendAnalysis analysis0
    where
      analysis0   = Analysis.singleton expression environment AbstractTop
      environment = Environment.map AbstractScalar
                  $ primitives `Environment.union` constants

iterateUntilStable :: Eq a => (a -> a) -> a -> [a]
iterateUntilStable f x = (x:) . map snd . takeWhile (uncurry (/=)) $ zs
    where
      ys = iterate f x
      zs = zip ys (tail ys)

interpret :: String -> String
interpret = render . pp . analyze . parse

interpret' :: String -> String
interpret' = unlines . map (render . pp) . analyze' . parse

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
