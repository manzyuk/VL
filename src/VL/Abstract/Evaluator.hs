module VL.Abstract.Evaluator where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Language.Pretty hiding (float)
import VL.Language.Read

import VL.Abstract.Value

import VL.Abstract.Analysis (AbstractAnalysis)
import qualified VL.Abstract.Analysis as Analysis

import Prelude hiding (read)

import Control.Monad
import System.IO
import Control.Exception

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
refineApply v _ _ = error $ "refineApply: can't refine " ++ show v

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
refinePrimitive Neg   _ = unary negate
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

refinePrimitive IfProc a = refineIfProc a

refinePrimitive IsNull _    = predicate isNull
refinePrimitive IsPair _    = predicate isPair
refinePrimitive IsReal _    = predicate isReal
refinePrimitive IsBoolean _ = predicate isBoolean

refinePrimitive RealPrim _  = primReal

primCar :: AbstractValue -> AbstractValue
primCar (AbstractPair v1 _) = v1
primCar AbstractBottom = AbstractBottom
primCar v = error $ "primCar: the argument is not a pair: " ++ show v

primCdr :: AbstractValue -> AbstractValue
primCdr (AbstractPair _ v2) = v2
primCdr AbstractBottom = AbstractBottom
primCdr v = error $ "primCdr: the argument is not a pair: " ++ show v

dyadic :: (AbstractValue -> AbstractValue -> AbstractValue)
       -> AbstractValue
       -> AbstractValue
dyadic op (AbstractPair v1 v2) = v1 `op` v2
dyadic _  AbstractBottom = AbstractBottom
dyadic _  v = error $ "dyadic: the argument is not a pair: " ++ show v

data Abstraction a
    = Abstraction {
      -- a string representation of the type a, for debugging
        name :: String
      -- view a concrete value of type a as an abstract value
      , convert  :: a -> AbstractValue
      -- try to extract a value of type a from an abstract value
      , extract  :: AbstractValue -> Maybe a
      -- abstract value corresponding to concrete values of type a
      , abstract :: AbstractValue
      }

float :: Abstraction Float
float = Abstraction {
          name     = "Real"
        , convert  = AbstractScalar . Real
        , extract  = maybeReal
        , abstract = AbstractReal
        }
    where
      maybeReal (AbstractScalar (Real r)) = Just r
      maybeReal _                         = Nothing

bool :: Abstraction Bool
bool = Abstraction {
         name     = "Bool"
       , convert  = AbstractScalar . Boolean
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
    = error $ unwords [ "liftOp: arguments"
                      , show x, "and", show y
                      , "do not match the expected types"
                      , name a, "and", name b
                      ]
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
unary _ v = error $ "unary: the argument is not a real: " ++ show v

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
refineIfProc _ AbstractBottom
    = AbstractBottom
refineIfProc _ v
    = error $ "refineIfProc: the argument is not a boolean: " ++ show v

refineThunk :: AbstractValue -> AbstractAnalysis -> AbstractValue
refineThunk thunk@(AbstractClosure _ _ _) a
    = refineApply thunk (AbstractScalar Nil) a
refineThunk v _
    = error $ "refineThunk: the argument is not a thunk: " ++ show v

predicate :: (AbstractValue -> Bool) -> AbstractValue -> AbstractValue
predicate _ AbstractBottom = AbstractBottom
predicate p v              = AbstractScalar (Boolean (p v))

isNull, isPair, isReal, isBoolean :: AbstractValue -> Bool
isNull (AbstractScalar Nil)            = True
isNull _                               = False
isPair (AbstractPair _ _)              = True
isPair _                               = False
isReal (AbstractScalar (Real _))       = True
isReal AbstractReal                    = True
isReal _                               = False
isBoolean (AbstractScalar (Boolean _)) = True
isBoolean AbstractBoolean              = True
isBoolean _                            = False

primReal :: AbstractValue -> AbstractValue
primReal (AbstractScalar (Real _)) = AbstractReal
primReal AbstractReal              = AbstractReal
primReal AbstractBottom               = AbstractBottom
primReal v = error $ "primReal: the argument is not a real " ++ show v

-- Originally, 'refineEval'' was called 'refineEval', and there was no
-- 'refineEval''.  However, Alexey and I have discovered recently that
-- so defined 'refineEval' is not monotonic and can lose information;
-- in particular, it can return AbstractBottom even for a binding that
-- is already known to evaluate to a non-bottom.  This has not bitten
-- us because in 'amendAnalysis' each binding is refined, and even if
-- 'refineEval' returns AbstractBottom for some binding, this is going
-- to be fixed by subsequent amendments, and the information necessary
-- for that is kept in the analysis.  We don't want to depend on that.
-- The right way to fix this problem is to really make 'refineEval'
-- monotonic, e.g., by taking the union of the old value of the
-- binding with the new, refined value.
refineEval :: CoreExpr
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractValue
refineEval e env a = Analysis.lookup e env a `joinValues` refineEval' e env a

refineEval' :: CoreExpr
            -> AbstractEnvironment
            -> AbstractAnalysis
            -> AbstractValue
refineEval' (Var x) env _ = Environment.lookup x env
refineEval' e@(Lam formal body) env _
    = AbstractClosure env' formal body
    where
      env' = Environment.restrict (freeVariables e) env
refineEval' (App operator operand) env a
    = refineApply (Analysis.lookup operator env a)
                  (Analysis.lookup operand  env a) a
refineEval' (Pair e1 e2) env a
    | v1 /= AbstractBottom && v2 /= AbstractBottom
    = AbstractPair v1 v2
    | otherwise
    = AbstractBottom
    where
      v1 = Analysis.lookup e1 env a
      v2 = Analysis.lookup e2 env a
refineEval' (Letrec bindings body) env a
    = refineEval' (pushLetrec bindings body) env a

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
expandApply v _ _ = error $ "expandApply: can't expand " ++ show v

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
expandIfProc v _
    = error $ "expandIfProc: the argument is not a boolean: " ++ show v

expandThunk :: AbstractValue -> AbstractAnalysis -> AbstractAnalysis
expandThunk thunk@(AbstractClosure _ _ _) a
    = expandApply thunk (AbstractScalar Nil) a
expandThunk v _
    = error $ "expandThunk: the argument is not a thunk: " ++ show v

expandEval :: CoreExpr
           -> AbstractEnvironment
           -> AbstractAnalysis
           -> AbstractAnalysis
expandEval (Var _)   _ _ = Analysis.empty
expandEval (Lam _ _) _ _ = Analysis.empty
expandEval (App operator operand) env a
    = Analysis.unions
      [ Analysis.expand operator env a
      , Analysis.expand operand  env a
      , expandApply (Analysis.lookup operator env a)
                    (Analysis.lookup operand  env a) a
      ]
expandEval (Pair e1 e2) env a
    = (Analysis.expand e1 env a) `Analysis.union` (Analysis.expand e2 env a)
expandEval (Letrec bindings body) env a
    = expandEval (pushLetrec bindings body) env a

amendAnalysis :: AbstractAnalysis -> AbstractAnalysis
amendAnalysis a = Analysis.unions . map amendBinding . Analysis.domain $ a
    where
      amendBinding (e, env)
          = Analysis.insert e env (refineEval e env a) (expandEval e env a)

-- NOTE: May not terminate
analyze :: (CoreExpr, ScalarEnvironment) -> AbstractAnalysis
analyze = last . analyze'

analyze' :: (CoreExpr, ScalarEnvironment) -> [AbstractAnalysis]
analyze' (expression, initialEnvironment)
    = iterateUntilStable amendAnalysis analysis0
    where
      analysis0   = Analysis.singleton expression environment AbstractBottom
      environment = abstractEnvironment initialEnvironment

iterateUntilStable :: Eq a => (a -> a) -> a -> [a]
iterateUntilStable f x = (x:) . map snd . takeWhile (uncurry (/=)) $ zs
    where
      ys = iterate f x
      zs = zip ys (tail ys)

abstractEnvironment :: ScalarEnvironment -> AbstractEnvironment
abstractEnvironment environment
    = Environment.map AbstractScalar environment

interpretMinimal :: String -> String
interpretMinimal input = pprint output
    where
      (expression, initialEnvironment) = read input
      environment = abstractEnvironment initialEnvironment
      analysis    = analyze (expression, initialEnvironment)
      output      = Analysis.lookup expression environment analysis

interpretCompact :: String -> String
interpretCompact = pprint . analyze . read

interpretVerbose :: String -> String
interpretVerbose = unlines . map pprint . analyze' . read

interpret :: String -> String
interpret = interpretMinimal

data Verbosity
    = Minimal        -- Print the abstract value of an expression only
    | Compact        -- Print the whole abstract analysis
    | Verbose        -- Print intermediate analyses

interpreter :: Verbosity -> IO ()
interpreter verbosity = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  forever . handle (\e -> print (e :: ErrorCall)) $ repl
    where
      repl = do
        putStr prompt
        input <- getLine
        putStrLn $ process input

      prompt = "vl> "
      process = case verbosity of
                  Minimal -> interpretMinimal
                  Compact -> interpretCompact
                  Verbose -> interpretVerbose
