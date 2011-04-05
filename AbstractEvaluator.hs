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
    = dispatch p a v
refineApply AbstractTop _ _ = AbstractTop
refineApply _ _ _ = error "Cannot refine an abstract non-function"

dispatch :: Primitive -> AbstractAnalysis -> AbstractValue -> AbstractValue
dispatch Car   _ = primCar
dispatch Cdr   _ = primCdr
dispatch Add   _ = arithmetic (+)
dispatch Sub   _ = arithmetic (-)
dispatch Mul   _ = arithmetic (*)
dispatch Div   _ = arithmetic (/)
dispatch Pow   _ = arithmetic (**)
dispatch Eql   _ = comparison (==)
dispatch Neq   _ = comparison (/=)
dispatch LTh   _ = comparison (<)
dispatch LEq   _ = comparison (<=)
dispatch GTh   _ = comparison (>)
dispatch GEq   _ = comparison (>=)
dispatch Exp   _ = unary exp
dispatch Log   _ = unary log
dispatch Sin   _ = unary sin
dispatch Cos   _ = unary cos
dispatch Tan   _ = unary tan
dispatch Sqrt  _ = unary sqrt
dispatch Asin  _ = unary asin
dispatch Acos  _ = unary acos
dispatch Atan  _ = unary atan
dispatch Sinh  _ = unary sinh
dispatch Cosh  _ = unary cosh
dispatch Tanh  _ = unary tanh
dispatch Asinh _ = unary asinh
dispatch Acosh _ = unary acosh
dispatch Atanh _ = unary atanh
dispatch Neg   _ = unary negate

dispatch IfProc a = primIfProc a

primCar :: AbstractValue -> AbstractValue
primCar (AbstractPair v1 _) = v1
primCar AbstractTop = AbstractTop
primCar _ = error "Provably a non-pair where a pair is expected"

primCdr :: AbstractValue -> AbstractValue
primCdr (AbstractPair _ v2) = v2
primCdr AbstractTop = AbstractTop
primCdr _ = error "Provably a non-pair where a pair is expected"

dyadic :: (AbstractValue -> AbstractValue -> AbstractValue)
       -> AbstractValue
       -> AbstractValue
dyadic op (AbstractPair v1 v2) = v1 `op` v2
dyadic op AbstractTop = AbstractTop
dyadic op _ = error "Provably a non-pair where a pair is expected"

liftOp :: (Float -> Float -> AbstractValue)
       -> AbstractValue
       -> (AbstractValue -> AbstractValue -> AbstractValue)
liftOp op c = l
    where
      l v1 v2 | isNotSomeReal v1 || isNotSomeReal v2
              = error "Provably a non-number where a number is expected"
              | v1 == AbstractTop || v2 == AbstractTop
              = AbstractTop
              | v1 == AbstractReal || v2 == AbstractReal
              = c
      l (AbstractScalar (Real r1)) (AbstractScalar (Real r2)) = r1 `op` r2

isNotSomeReal :: AbstractValue -> Bool
isNotSomeReal = not . isSomeReal

isSomeReal :: AbstractValue -> Bool
isSomeReal (AbstractScalar (Real _)) = True
isSomeReal AbstractReal              = True
isSomeReal AbstractTop               = True
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
unary _ _ = error "Provably a non-number where a number is expected"

primIfProc :: AbstractAnalysis -> AbstractValue -> AbstractValue
primIfProc a (AbstractPair (AbstractScalar (Boolean c))
                           (AbstractPair t e))
    | c
    = refineThunk t a
    | otherwise
    = refineThunk e a
primIfProc a (AbstractPair (AbstractScalar (Boolean _))
                           AbstractTop)
    = AbstractTop
primIfProc a (AbstractPair AbstractBoolean
                           (AbstractPair t e))
    = (refineThunk t a) `unifyValues` (refineThunk e a)
primIfProc a (AbstractPair AbstractBoolean
                           AbstractTop)
    = AbstractTop
primIfProc a (AbstractPair AbstractTop _)
    = AbstractTop
primIfProc a AbstractTop
    = AbstractTop
primIfProc a _
    = error "Provably a non-boolean where a boolean is expected"

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
expandApply (AbstractScalar (Primitive IfProc)) v a
    = expandIfProc v a
expandApply (AbstractScalar (Primitive p)) _ _
    = Analysis.empty
expandApply AbstractTop _ _ = Analysis.empty
expandApply _ _ _ = error "Cannot expand an abstract non-function"

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
expandIfProc _ _ = error "Malformed `if'"

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
    = Analysis.unions [ Analysis.expand e1 env a
                      , Analysis.expand e2 env a
                      , expandApply (Analysis.lookup e1 env a) (Analysis.lookup e2 env a) a
                      ]
expandEval (Cons e1 e2) env a
    = (Analysis.expand e1 env a) `Analysis.union` (Analysis.expand e2 env a)

u :: AbstractAnalysis -> AbstractAnalysis
u a = Analysis.unions . map u1 . Analysis.domain $ a
    where
      u1 (e, env) = Analysis.insert e env (refineEval e env a) (expandEval e env a)

-- NOTE: May not terminate
analyze :: (CoreExpression , ScalarEnvironment) -> AbstractAnalysis
analyze (expression, constants) = leastFixedPoint u analysis0
    where
      analysis0   = Analysis.singleton expression environment AbstractTop
      environment = Environment.map AbstractScalar
                  $ primitives `Environment.union` constants

analyze' :: (CoreExpression , ScalarEnvironment) -> [AbstractAnalysis]
analyze' (expression, constants) = leastFixedPoint' u analysis0
    where
      analysis0   = Analysis.singleton expression environment AbstractTop
      environment = Environment.map AbstractScalar
                  $ primitives `Environment.union` constants

leastFixedPoint :: Eq a => (a -> a) -> a -> a
leastFixedPoint f x | f x == x  = x
                    | otherwise = leastFixedPoint f (f x)

leastFixedPoint' :: Eq a => (a -> a) -> a -> [a]
leastFixedPoint' f x = (x:) . map snd . takeWhile (uncurry (/=)) $ zs
    where
      ys = iterate f x
      zs = zip ys (tail ys)

interpret :: String -> String
interpret = render . pp . analyze . parse

interpret' :: String -> String
interpret' = unlines . map (render . pp) . analyze' . parse

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

interpreter' :: IO ()
interpreter' = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever repl
    where
      repl = do
        putStr prompt
        input <- getLine
        putStrLn $ interpret' input

      prompt = "vl> "
