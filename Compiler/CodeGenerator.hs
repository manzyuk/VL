{-# LANGUAGE PatternGuards #-}
module VL.Compiler.CodeGenerator where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Abstract.Value

import VL.Abstract.Analysis (AbstractAnalysis)
import qualified VL.Abstract.Analysis as Analysis

import VL.Abstract.Evaluator hiding (float, unary, arithmetic, comparison)

import VL.Language.Parser (parse)
import VL.Language.Pretty
import VL.Language.Prepare

import VL.Language.Uniquify

import VL.Compiler.C
import VL.Compiler.ZEncoding

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

import Control.Monad
import Control.Arrow (first, second)

import Debug.Trace

-- Given a lookup /table/, return a lookup /function/.  The lookup
-- function signals an error when the key is not found.
mkLookupFun :: (Ord k, Show k) => Map k v -> (k -> v)
mkLookupFun table key = fromMaybe (error msg) (Map.lookup key table)
    where
      msg = "mkLookupFun: key is not found: " ++ show key

-- Given a list of things, build a lookup table, giving each thing
-- a unique name based on a prefix supplied as the first argument.
mkLookupTbl :: Ord k => String -> [k] -> Map k Name
mkLookupTbl prefix keys
    = evalSupply $ do
        alist <- sequence [ liftM2 (,) (return k) (freshName prefix)
                          | k <- keys
                          ]
        let table = Map.fromList $ map (second zencode) alist
        return table

-- Given a map from abstract values to C identifiers and a map from
-- VL variables to C identifiers, build a C type corresponding to
-- a given abstract value.
genCType :: (AbstractValue -> Name)
         -> (Name -> Name)
         -> AbstractValue
         -> CType
genCType _    _    (AbstractScalar (Boolean _))     = CInt
genCType _    _    AbstractBoolean                  = CInt
genCType _    _    (AbstractScalar (Real _))        = CDouble
genCType _    _    AbstractReal                     = CDouble
genCType sFun _    v@(AbstractScalar (Primitive _)) = CStruct (sFun v) []
genCType sFun _    v@(AbstractScalar Nil)           = CStruct (sFun v) []
genCType sFun xFun v@(AbstractClosure env _ _)      = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, xFun x)
           | (x, v) <- Environment.bindings env
           ]
genCType sFun xFun v@(AbstractPair car cdr)         = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, x)
           | (x, v) <- [("a", car), ("d", cdr)]
           ]

-- Given an abstract value, if its associated C type is a struct,
-- generate a code that declares that struct type and defines a
-- constructor for it.
genCStructDecl :: (AbstractValue -> CType)
               -> (AbstractValue -> Name)
               -> AbstractValue
               -> CProg
genCStructDecl tFun mFun v
    | typ@(CStruct name slots) <- tFun v
    = [ CStructDecl name slots
      , CFunDecl (CFunProto typ (mFun v) slots)
                 [ CLocalVarDecl typ "result"
                     (CStructCon . map (CVar . snd) $ slots)
                 , CReturn (CVar "result")
                 ]
      ]
    | otherwise = []

-- Turn a concrete value into a C value.
genCValue :: AbstractValue -> CExpr
genCValue (AbstractScalar Nil)             = CStructCon []
genCValue (AbstractScalar (Boolean True))  = CIntLit 1
genCValue (AbstractScalar (Boolean False)) = CIntLit 0
genCValue (AbstractScalar (Real r))        = CDoubleLit r
genCValue (AbstractScalar (Primitive _))   = CStructCon []
genCValue (AbstractClosure _ _ _ )         = error "genCValue: AbstractClosure"
genCValue (AbstractPair _ _)               = error "genCValue: AbstractPair"
genCValue AbstractBoolean                  = error "genCValue: AbstractBoolean"
genCValue AbstractReal                     = error "genCValue: AbstractReal"
genCValue AbstractBottom                   = error "genCValue: AbstractBottom"

-- Given a name and a concrete value it is bound to,  generate a code
-- for declaring a global variable with the given name and initiating
-- to the corresponding C value.
genCGlobalVarDecl :: (AbstractValue -> CType)
                  -> Name
                  -> AbstractValue
                  -> CDecl
genCGlobalVarDecl tFun x v
    = CGlobalVarDecl (tFun v) x (genCValue v)

-- 'genCExpr' is C from the paper
--
-- The 6th argument is a list of variables that are free in the given
-- context.  Because 'genCExpr' is only called to generate bodies of
-- the functions corresponding to closure applications, this argument
-- will usually be the list of variables bound by the environment of
-- the closure, or the empty list for the 'main' function.
genCExpr :: (Name -> Name)                           -- X
         -> (AbstractValue -> Name)                  -- M
         -> ((AbstractValue, AbstractValue) -> Name) -- F
         -> AbstractAnalysis                         -- a*
         -> AbstractEnvironment
         -> [Name]
         -> CoreExpr
         -> CExpr
genCExpr xFun mFun fFun a env fvs (Var x)
    | x `elem` fvs
    = CSlotAccess "c" (xFun x)
    | otherwise
    = CVar (xFun x)
genCExpr xFun mFun fFun a env fvs e@(Lam _ _)
    = CFunCall name args
    where
      name = mFun $ Analysis.lookup e env a
      args = [ CVar (zencode x)
             | (x, v) <- Environment.bindings $ Environment.restrict (freeVariables e) env
             ]
genCExpr xFun mFun fFun a env fvs (App e1 e2)
    = CFunCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = fFun (v1, v2)
      args = [ genCExpr xFun mFun fFun a env fvs o
             | (o, v) <- [(e1, v1), (e2, v2)]
             ]
genCExpr xFun mFun fFun a env fvs (Pair e1 e2)
    = CFunCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = mFun $ AbstractPair v1 v2
      args = [ genCExpr xFun mFun fFun a env fvs e
             | (e, v) <- [(e1, v1), (e2, v2)]
             ]
genCExpr xFun mFun fFun a env fvs (Letrec bindings body)
    = error "genCExpr: LETREC is not supported yet"

-- Generate a prototype declaration and a definition of the function
-- corresponding to the application of an abstract closure (given by
-- its abstract environment, name of the formal parameter and body)
-- to an abstract value.
genFunDecl :: (Name -> Name)                           -- X
           -> (AbstractValue -> CType)                 -- T
           -> (AbstractValue -> Name)                  -- M
           -> ((AbstractValue, AbstractValue) -> Name) -- F
           -> AbstractAnalysis                         -- a*
           -> AbstractValue
           -> AbstractEnvironment
           -> Name
           -> CoreExpr
           -> (CDecl, CDecl)
genFunDecl xFun tFun mFun fFun a v env x e
    = (CFunProtoDecl proto, CFunDecl proto body)
    where
      c       = AbstractClosure env x e
      ty      = tFun $ refineApply c v a
      name    = fFun (c, v)
      formals = [ (tFun v, p) | (v, p) <- [(c, "c"), (v, xFun x)] ]
      body    = [ CReturn $ genCExpr xFun mFun fFun a (Environment.insert x v env) (Environment.domain env) e ]
      proto   = CFunProto ty name formals

-- Ditto for the application of a primitive to an abstract value.
genPrimDecl :: (Name -> Name)
            -> (AbstractValue -> CType)
            -> (AbstractValue -> Name)
            -> ((AbstractValue, AbstractValue) -> Name)
            -> AbstractAnalysis
            -> AbstractValue
            -> Primitive
            -> (CDecl, CDecl)
genPrimDecl xFun tFun mFun fFun a v p
    = (CFunProtoDecl proto, CFunDecl proto body)
    where
      f    = AbstractScalar (Primitive p)
      typ  = tFun $ refineApply f v a
      name = fFun (f, v)
      formals = [ (tFun f, "f"), (tFun v, "x") ]
      body = [ CReturn $ applyPrimitive p v "x" ]
      proto = CFunProto typ name formals

applyPrimitive :: Primitive -> AbstractValue -> Name -> CExpr
applyPrimitive Car      = car
applyPrimitive Cdr      = cdr
applyPrimitive Add      = arithmetic (+) "+"
applyPrimitive Sub      = arithmetic (-) "-"
applyPrimitive Mul      = arithmetic (*) "*"
applyPrimitive Div      = arithmetic (/) "/"
applyPrimitive Eql      = comparison (==) "=="
applyPrimitive Neq      = comparison (/=) "!="
applyPrimitive LTh      = comparison (<)  "<"
applyPrimitive LEq      = comparison (<=) "<="
applyPrimitive GTh      = comparison (>)  ">"
applyPrimitive GEq      = comparison (>=) ">="
applyPrimitive Exp      = unary exp  "exp"
applyPrimitive Log      = unary log  "log"
applyPrimitive Sin      = unary sin  "sin"
applyPrimitive Cos      = unary cos  "cos"
applyPrimitive Tan      = unary tan  "tan"
applyPrimitive Asin     = unary asin "asin"
applyPrimitive Acos     = unary acos "acos"
applyPrimitive Atan     = unary atan "atan"
applyPrimitive Sinh     = unary sinh "sinh"
applyPrimitive Cosh     = unary cosh "cosh"
applyPrimitive Tanh     = unary tanh "tanh"
applyPrimitive Sqrt     = unary sqrt "sqrt"
applyPrimitive Pow      = pow
applyPrimitive RealPrim = realPrim

car :: AbstractValue -> Name -> CExpr
car (AbstractPair v@(AbstractScalar _) _) _ = genCValue v
car _ x = CSlotAccess x "a"

cdr :: AbstractValue -> Name -> CExpr
cdr (AbstractPair _ v@(AbstractScalar _)) _ = genCValue v
cdr _ x = CSlotAccess x "d"

pow :: AbstractValue -> Name -> CExpr
pow (AbstractPair (AbstractScalar (Real r1))
                  (AbstractScalar (Real r2))) _
    = CDoubleLit (r1 ** r2)
pow v x = CFunCall "pow" [car v x, cdr v x]

unary :: (Float -> Float) -> Name -> AbstractValue -> Name -> CExpr
unary fun fun_name (AbstractScalar (Real r)) _
    = CDoubleLit (fun r)
unary _ fun_name _ x = CFunCall fun_name [CVar x]

arithmetic :: (Float -> Float -> Float)
           -> Name
           -> AbstractValue
           -> Name
           -> CExpr
arithmetic op op_name (AbstractPair (AbstractScalar (Real r1))
                                    (AbstractScalar (Real r2))) _
    = CDoubleLit (r1 `op` r2)
arithmetic op op_name v x
    = CBinaryOp op_name (car v x) (cdr v x)

comparison :: (Float -> Float -> Bool)
           -> Name
           -> AbstractValue
           -> Name
           -> CExpr
comparison op op_name (AbstractPair (AbstractScalar (Real r1))
                                    (AbstractScalar (Real r2))) _
    = CIntLit . bool2int $ r1 `op` r2
    where
      bool2int True  = 1
      bool2int False = 0

realPrim :: AbstractValue -> Name -> CExpr
realPrim (AbstractScalar (Real r)) _ = CDoubleLit r
realPrim _ x = CVar x

genCProg :: (CoreExpr, ScalarEnvironment) -> CProg
genCProg program@(expression, constants)
    = structDecls ++ funProtos ++ primProtos ++ globals ++ [entryPoint] ++ funDecls ++ primDecls
    where
      analysis = analyze program
      values   = nub (Analysis.values analysis)

      -- Build lookup tables and lookup functions for relevant data
      xTbl = Map.fromList [(n, zencode n) | n <- Set.toList (variables expression)]
      xFun = mkLookupFun xTbl   -- X from the paper
      sTbl = mkLookupTbl "#:val-" values
      sFun = mkLookupFun sTbl   -- S from the paper
      fTbl = mkLookupTbl "#:fun-" [ (v1, v2) | v1 <- values, v2 <- values ]
      fFun = mkLookupFun fTbl   -- F from the paper
      tTbl = Map.fromList [(v, genCType sFun xFun v) | v <- values ]
      tFun = mkLookupFun tTbl   -- T from the paper
      mTbl = mkLookupTbl "#:con-" values
      mFun = mkLookupFun mTbl   -- M from the paper

      closureValuePairs
          = nub [ (env', x, b, v)
                | (App l@(Lam x b) e, env) <- Analysis.domain analysis
                , let env' = Environment.restrict (freeVariables l) env
                      v    = Analysis.lookup e env analysis
                ]
      primValuePairs
          = nub [ (p, v)
                | (App (Var f) e, env) <- Analysis.domain analysis
                , AbstractScalar (Primitive p) <- [Environment.lookup f env]
                , let v = Analysis.lookup e env analysis
                ]

      structDecls
          = concatMap (genCStructDecl tFun mFun) values
      (funProtos, funDecls)
          = unzip [ genFunDecl xFun tFun mFun fFun analysis v env x e
                  | (env, x, e, v) <- closureValuePairs
                  ]
      (primProtos, primDecls)
          = unzip [ genPrimDecl xFun tFun mFun fFun analysis v p
                  | (p, v) <- primValuePairs
                  ]
      entryPoint
          = CFunDecl (CFunProto CInt "main" [])
            [ CLocalVarDecl (tFun result) "result" (genCExpr xFun mFun fFun analysis environment [] expression)
            , CReturn (CIntLit 0)
            ]
      globals
          = [ genCGlobalVarDecl tFun (zencode x) v
            | (x, v) <- Environment.bindings . Environment.restrict (freeVariables expression) $ environment
            ]
      environment = initialAbstractEnvironment constants
      result      = Analysis.lookup expression environment analysis
