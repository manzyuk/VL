module VL.Compiler.CodeGenerator where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Language.Read
import VL.Language.Pretty

import VL.Abstract.Value
import VL.Abstract.Evaluator hiding (arithmetic, comparison, unary)

import VL.Abstract.Analysis (AbstractAnalysis)
import qualified VL.Abstract.Analysis as Analysis

import VL.Compiler.C
import VL.Compiler.ZEncoding

import Prelude hiding (read)

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Set as Set

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Control.Applicative

import Debug.Trace

data Table = Table { strName :: Map AbstractValue Name
                   , conName :: Map AbstractValue Name
                   , thkName :: Map AbstractValue Name
                   , appName :: Map (AbstractValue, AbstractValue) Name
                   , counter :: Int
                   }

type CG = ReaderT AbstractAnalysis (State Table)

runCG :: CG a -> AbstractAnalysis -> a
runCG = (flip evalState emptyTable .). runReaderT
    where
      emptyTable = Table { strName = Map.empty
                         , conName = Map.empty
                         , thkName = Map.empty
                         , appName = Map.empty
                         , counter = 0
                         }

analysis :: CG AbstractAnalysis
analysis = ask

getStrName :: AbstractValue -> CG Name
getStrName v
    = do table <- get
         case Map.lookup v (strName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:str-" ++ show i
                      in do put table { strName = Map.insert v n (strName table)
                                      , counter = succ i
                                      }
                            return n

getConName :: AbstractValue -> CG Name
getConName v
    = do table <- get
         case Map.lookup v (conName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:con-" ++ show i
                      in do put table { conName = Map.insert v n (conName table)
                                      , counter = succ i
                                      }
                            return n

getThkName :: AbstractValue -> CG Name
getThkName v
    = do table <- get
         case Map.lookup v (thkName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:thk-" ++ show i
                      in do put table { thkName = Map.insert v n (thkName table)
                                      , counter = succ i
                                      }
                            return n

getAppName :: AbstractValue -> AbstractValue -> CG Name
getAppName v1 v2
    = do table <- get
         case Map.lookup (v1, v2) (appName table) of
           Just n  -> return n
           Nothing -> let i = counter table
                          n = zencode $ "#:app-" ++ show i
                      in do put table { appName = Map.insert (v1, v2) n (appName table)
                                      , counter = succ i
                                      }
                            return n

typeOf :: AbstractValue -> CG CType
typeOf (AbstractScalar (Boolean _)) = return CInt
typeOf AbstractBoolean              = return CInt
typeOf (AbstractScalar (Real _))    = return CDouble
typeOf AbstractReal                 = return CDouble
typeOf u@(AbstractScalar (Primitive _))
    = do str_name <- getStrName u
         return $ CStruct str_name []
typeOf u@(AbstractScalar Nil)
    = do str_name <- getStrName u
         return $ CStruct str_name []
typeOf u@(AbstractClosure env _ _)
    = do str_name <- getStrName u
         members  <- sequence [ liftM2 (,) (typeOf v) (return $ zencode x)
                              | (x, v) <- Environment.bindings env
                              ]
         return $ CStruct str_name members
typeOf u@(AbstractPair car cdr)
    = do str_name <- getStrName u
         members  <- sequence [ liftM2 (,) (typeOf v) (return $ zencode x)
                              | (x, v) <- [("a", car), ("d", cdr)]
                              ]
         return $ CStruct str_name members
typeOf AbstractBottom = error "typeOf: AbstractBottom"

genCStructDecl :: AbstractValue -> CG CProg
genCStructDecl v
    = do typ <- typeOf v
         case typ of
           CStruct str_name members ->
               do con_name <- getConName v
                  let proto = CFunProto typ con_name members
                      body  = [ CLocalVarDecl typ "result" . CStructCon $
                                [ CVar var | (_, var) <- members ]
                              , CReturn (CVar "result")
                              ]
                  return [CStructDecl str_name members, CFunDecl proto body]
           _ -> return []

valueOf :: AbstractValue -> CExpr
valueOf (AbstractScalar s) = case s of
                               Nil           -> CStructCon []
                               Boolean True  -> CIntLit 1
                               Boolean False -> CIntLit 0
                               Real r        -> CDoubleLit r
                               Primitive _   -> CStructCon []
valueOf _ = error "valueOf: non-solved abstract value"

genCGlobalVarDecl :: Name -> AbstractValue -> CG CDecl
genCGlobalVarDecl x v
    = do typ <- typeOf v
         return $ CGlobalVarDecl typ (zencode x) (valueOf v)

genCExpr :: CoreExpr -> AbstractEnvironment -> [Name] -> CG CExpr
genCExpr (Var x) env fvs
    | x `elem` fvs
    = return $ CSlotAccess (CVar "c") (zencode x)
    | otherwise
    = return $ CVar (zencode x)
genCExpr e@(Lam _ _) env fvs
    = do closure@(AbstractClosure closure_env _ _) <- Analysis.lookup e env <$> analysis
         fun_name <- getConName closure
         args <- sequence [genCExpr (Var x) env fvs | x <- Environment.domain closure_env]
         return $ CFunCall fun_name args
genCExpr (App e1 e2) env fvs
    = do v1 <- Analysis.lookup e1 env <$> analysis
         v2 <- Analysis.lookup e2 env <$> analysis
         fun_name <- getAppName v1 v2
         c1 <- genCExpr e1 env fvs
         c2 <- genCExpr e2 env fvs
         return $ CFunCall fun_name [c1, c2]
genCExpr e@(Pair e1 e2) env fvs
    = do pair <- Analysis.lookup e env <$> analysis
         fun_name <- getConName pair
         c1 <- genCExpr e1 env fvs
         c2 <- genCExpr e2 env fvs
         return $ CFunCall fun_name [c1, c2]
genCExpr (Letrec bindings body) env fvs
    = error "genCExpr: LETREC is not supported yet"

-- Closure applications
type ClosureValuePair = (AbstractEnvironment, Name, CoreExpr, AbstractValue)

compileClosureApplication :: ClosureValuePair -> CG (CDecl, CDecl)
compileClosureApplication (env, x, e, v)
    = do ret_type <- typeOf =<< (refineApply closure v <$> analysis)
         fun_name <- getAppName closure v
         formals  <- sequence [ liftM2 (,) (typeOf val) (return var)
                              | (val, var) <- [(closure, "c"), (v, zencode x)]
                              ]
         ret_stat <- CReturn <$> genCExpr e (Environment.insert x v env) (Environment.domain env)
         let proto = CFunProto ret_type fun_name formals
         return (CFunProtoDecl proto, CFunDecl proto [ret_stat])
    where
      closure = AbstractClosure env x e

enumClosureValuePairs :: CG [ClosureValuePair]
enumClosureValuePairs
    = do a <- analysis
         return $ nub [ (closure_env, closure_formal, closure_body, operand_value)
                      | (App operator@(Lam _ _) operand, env) <- Analysis.domain a
                      , let (AbstractClosure closure_env closure_formal closure_body)
                              = Analysis.lookup operator env a
                            operand_value
                              = Analysis.lookup operand  env a
                      ]

compileClosureApplications :: CG [(CDecl, CDecl)]
compileClosureApplications = enumClosureValuePairs >>= mapM compileClosureApplication

-- Primitive applications
type PrimitiveValuePair = (Primitive, AbstractValue)

compilePrimitiveApplication :: PrimitiveValuePair -> CG (CDecl, CDecl)
compilePrimitiveApplication (p, v)
    = do ret_type <- typeOf =<< (refineApply primitive v <$> analysis)
         fun_name <- getAppName primitive v
         formals  <- sequence [ liftM2 (,) (typeOf val) (return var)
                              | (val, var) <- [(primitive, "dummy"), (v, "x")]
                              ]
         ret_stat <- CReturn <$> compilePrimitiveExpr p v "x"
         let proto = CFunProto ret_type fun_name formals
         return (CFunProtoDecl proto, CFunDecl proto [ret_stat])
    where
      primitive = AbstractScalar (Primitive p)

enumPrimitiveValuePairs :: CG [PrimitiveValuePair]
enumPrimitiveValuePairs
    = do a <- analysis
         return $ nub [ (primitive, operand_value)
                      | (App (Var p) operand, env) <- Analysis.domain a
                      , AbstractScalar (Primitive primitive) <- [Environment.lookup p env]
                      , let operand_value
                              = Analysis.lookup operand  env a
                      ]

compilePrimitiveApplications :: CG [(CDecl, CDecl)]
compilePrimitiveApplications = enumPrimitiveValuePairs >>= mapM compilePrimitiveApplication

compilePrimitiveExpr :: Primitive -> AbstractValue -> Name -> CG CExpr
compilePrimitiveExpr Car = car
compilePrimitiveExpr Cdr = cdr
compilePrimitiveExpr Add = arithmetic (+) "+"
compilePrimitiveExpr Sub = arithmetic (-) "-"
compilePrimitiveExpr Mul = arithmetic (*) "*"
compilePrimitiveExpr Div = arithmetic (/) "/"
compilePrimitiveExpr Eql = comparison (==) "=="
compilePrimitiveExpr Neq = comparison (/=) "!="
compilePrimitiveExpr LTh = comparison (<)  "<"
compilePrimitiveExpr LEq = comparison (<=) "<="
compilePrimitiveExpr GTh = comparison (>)  ">"
compilePrimitiveExpr GEq = comparison (>=) ">="
compilePrimitiveExpr Exp = unary exp "exp"
compilePrimitiveExpr Log = unary log "log"
compilePrimitiveExpr Sin = unary sin "sin"
compilePrimitiveExpr Cos = unary cos "cos"
compilePrimitiveExpr Tan = unary tan "tan"
compilePrimitiveExpr Asin = unary asin "asin"
compilePrimitiveExpr Acos = unary acos "acos"
compilePrimitiveExpr Atan = unary atan "atan"
compilePrimitiveExpr Sinh = unary sinh "sinh"
compilePrimitiveExpr Cosh = unary cosh "cosh"
compilePrimitiveExpr Tanh = unary tanh "tanh"
compilePrimitiveExpr Sqrt = unary sqrt "sqrt"
compilePrimitiveExpr Pow  = pow
compilePrimitiveExpr RealPrim = realPrim
compilePrimitiveExpr IfProc = ifProc

car :: AbstractValue -> Name -> CG CExpr
car (AbstractPair v@(AbstractScalar _) _) _ = return $ valueOf v
car _                                     x = return $ CSlotAccess (CVar x) "a"

cdr :: AbstractValue -> Name -> CG CExpr
cdr (AbstractPair _ v@(AbstractScalar _)) _ = return $ valueOf v
cdr _                                     x = return $ CSlotAccess (CVar x) "d"

arithmetic :: (Float -> Float -> Float)
           -> Name
           -> AbstractValue
           -> Name
           -> CG CExpr
arithmetic op op_name (AbstractPair (AbstractScalar (Real r1))
                                    (AbstractScalar (Real r2))) _
    = return $ CDoubleLit (r1 `op` r2)
arithmetic op op_name v x
    = liftM2 (CBinaryOp op_name) (car v x) (cdr v x)

comparison :: (Float -> Float -> Bool)
           -> Name
           -> AbstractValue
           -> Name
           -> CG CExpr
comparison op op_name (AbstractPair (AbstractScalar (Real r1))
                                    (AbstractScalar (Real r2))) _
    = return . CIntLit . bool2int $ r1 `op` r2
    where
      bool2int True  = 1
      bool2int False = 0
comparison _ op_name v x
    = liftM2 (CBinaryOp op_name) (car v x) (cdr v x)

unary :: (Float -> Float) -> Name -> AbstractValue -> Name -> CG CExpr
unary fun fun_name (AbstractScalar (Real r)) _
    = return $ CDoubleLit (fun r)
unary _ fun_name _ x = return $ CFunCall fun_name [CVar x]

pow :: AbstractValue -> Name -> CG CExpr
pow (AbstractPair (AbstractScalar (Real r1))
                  (AbstractScalar (Real r2))) _
    = return $ CDoubleLit (r1 ** r2)
pow v x = (CFunCall "pow") <$> sequence [car v x, cdr v x]

realPrim :: AbstractValue -> Name -> CG CExpr
realPrim (AbstractScalar (Real r)) _ = return $ CDoubleLit r
realPrim _                         x = return $ CVar x

ifProc :: AbstractValue -> Name -> CG CExpr
ifProc (AbstractPair (AbstractScalar (Boolean True))
                     (AbstractPair thunk _)) x
    = do thk_name <- getThkName thunk
         return $ CFunCall thk_name [cdar x]
ifProc (AbstractPair (AbstractScalar (Boolean False))
                     (AbstractPair _ thunk)) x
    = do thk_name <- getThkName thunk
         return $ CFunCall thk_name [cddr x]
ifProc (AbstractPair AbstractBoolean
                     (AbstractPair thunk1 thunk2)) x
    = do thk_name1 <- getThkName thunk1
         thk_name2 <- getThkName thunk2
         return $ CTernaryCond (CSlotAccess (CVar x) "a")
                    (CFunCall thk_name1 [cdar x])
                    (CFunCall thk_name2 [cddr x])

cdar :: Name -> CExpr
cdar x = CSlotAccess (CSlotAccess (CVar x) "d") "a"

cddr :: Name -> CExpr
cddr x = CSlotAccess (CSlotAccess (CVar x) "d") "d"

-- Thunks
type Thunk = (AbstractEnvironment, Name, CoreExpr)

compileThunk :: Thunk -> CG (CDecl, CDecl)
compileThunk (env, x, e)
    = do ret_type <- typeOf =<< (refineEval e env <$> analysis)
         fun_name <- getThkName thunk
         thk_type <- typeOf thunk
         let formals = [(thk_type, "c")]
         ret_stat <- CReturn <$> genCExpr e env (Environment.domain env)
         let proto = CFunProto ret_type fun_name formals
         return (CFunProtoDecl proto, CFunDecl proto [ret_stat])
    where
      thunk = AbstractClosure env x e

enumThunks :: CG [Thunk]
enumThunks
    = do a <- analysis
         return $ nub [ (env, x, e)
                      | ((Lam x e, _), AbstractClosure env _ _) <- Analysis.toList a
                      , not (x `Set.member` freeVariables e)
                      ]

compileThunks :: CG [(CDecl, CDecl)]
compileThunks = enumThunks >>= mapM compileThunk

-- Compile main function
compileMain :: CoreExpr -> AbstractEnvironment -> CG CDecl
compileMain expression environment
    = do res      <- Analysis.lookup expression environment <$> analysis
         res_type <- typeOf res
         res_expr <- genCExpr expression environment []
         let proto = CFunProto CInt "main" []
             body  = [ CLocalVarDecl res_type "result" res_expr
                     , CReturn (CIntLit 0)
                     ]
         return $ CFunDecl proto body

genCProg :: (CoreExpr, ScalarEnvironment) -> CProg
genCProg program@(expression, initialEnvironment)
    = runCG code analysis
    where
      environment = abstractEnvironment initialEnvironment
      analysis    = analyze program
      values      = nub (Analysis.values analysis)
      code        = do structs <- concatMapM genCStructDecl values
                       globals <- sequence [ genCGlobalVarDecl x v
                                           | (x, v) <- Environment.bindings environment
                                           ]
                       closures <- compileClosureApplications
                       primitives <- compilePrimitiveApplications
                       thunks <- compileThunks
                       entry <- compileMain expression environment
                       let (closure_protos, closure_defns) = unzip closures
                           (primitive_protos, primitive_defns) = unzip primitives
                           (thunk_protos, thunk_defns) = unzip thunks
                           protos = closure_protos ++ primitive_protos ++ thunk_protos
                           defns = closure_defns ++ primitive_defns ++ thunk_defns
                       return $ structs ++ globals ++ protos ++ [entry] ++ defns

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . sequence . map f