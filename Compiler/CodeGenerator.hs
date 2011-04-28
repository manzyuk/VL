module VL.Compiler.CodeGenerator where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Expression

import qualified VL.Language.Environment as Environment

import VL.Language.Read

import VL.Abstract.Value
import VL.Abstract.Evaluator

import VL.Abstract.Analysis (AbstractAnalysis)
import qualified VL.Abstract.Analysis as Analysis

import VL.Compiler.C
import VL.Compiler.Monad
import VL.Compiler.ZEncoding

import Prelude hiding (read)

import Data.Ord
import Data.List

import Control.Monad
import Control.Applicative

typeOf :: AbstractValue -> CG CType
typeOf (AbstractScalar (Boolean _)) = return CInt
typeOf AbstractBoolean              = return CInt
typeOf (AbstractScalar (Real _))    = return CDouble
typeOf AbstractReal                 = return CDouble
typeOf u@(AbstractScalar (Primitive _))
    = do (str_name, str_index) <- getStrNameAndIndex u
         return $ CStruct str_name [] str_index
typeOf u@(AbstractScalar Nil)
    = do (str_name, str_index) <- getStrNameAndIndex u
         return $ CStruct str_name [] str_index
-- NOTE:  It is important that 'getStrNameAndIndex' below is called
-- /after/ determining the types of the struct members.  This ensures
-- that the index of a struct is always greater than the indexes of
-- the structs it depends upon.  This allows us to achieve the result
-- of topological ordering of the structs by merely sorting them by
-- their indexes.
typeOf u@(AbstractClosure env _ _)
    = do members  <- sequence [ liftM2 (,) (typeOf v) (return $ zencode x)
                              | (x, v) <- Environment.bindings env
                              ]
         (str_name, str_index) <- getStrNameAndIndex u
         return $ CStruct str_name members str_index
typeOf u@(AbstractPair car cdr)
    = do members  <- sequence [ liftM2 (,) (typeOf v) (return $ zencode x)
                              | (x, v) <- [("a", car), ("d", cdr)]
                              ]
         (str_name, str_index) <- getStrNameAndIndex u
         return $ CStruct str_name members str_index
typeOf AbstractBottom = error "typeOf: AbstractBottom"

compileStructDecl :: AbstractValue -> CG [(CDecl, CDecl)]
compileStructDecl v
    = do typ <- typeOf v
         case typ of
           CStruct str_name members str_index ->
               do con_name <- getConName v
                  let proto = CFunProto typ con_name members
                      body  = [ CLocalVarDecl typ "result" . CStructCon $
                                [ CVar var | (_, var) <- members ]
                              , CReturn (CVar "result")
                              ]
                  return [(CStructDecl str_name members str_index, CFunDecl proto body)]
           _ -> return []

compileStructDecls :: CG [(CDecl, CDecl)]
compileStructDecls
    = concatMapM compileStructDecl =<< Analysis.values <$> analysis

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . sequence . map f

ctrue, cfalse, cempty :: CExpr
ctrue     = CIntLit 1
cfalse    = CIntLit 0
cempty    = CStructCon []

valueOf :: AbstractValue -> CExpr
valueOf (AbstractScalar s)
    = case s of
        Nil           -> cempty
        Boolean True  -> ctrue
        Boolean False -> cfalse
        Real r        -> CDoubleLit r
        Primitive _   -> cempty
valueOf _ = error "valueOf: non-scalar value"

compileGlobalVarDecl :: Name -> AbstractValue -> CG CDecl
compileGlobalVarDecl x v
    = do typ <- typeOf v
         return $ CGlobalVarDecl typ (zencode x) (valueOf v)

compileGlobalVarDecls :: AbstractEnvironment -> CG [CDecl]
compileGlobalVarDecls env
    = sequence [ compileGlobalVarDecl x v
               | (x, v) <- Environment.bindings env
               ]

compileExpr :: CoreExpr -> AbstractEnvironment -> [Name] -> CG CExpr
compileExpr (Var x) _ fvs
    | x `elem` fvs
    = return $ CSlotAccess (CVar "c") (zencode x)
    | otherwise
    = return $ CVar (zencode x)
compileExpr e@(Lam _ _) env fvs
    = do closure@(AbstractClosure closure_env _ _) <- Analysis.lookup e env <$> analysis
         fun_name <- getConName closure
         args <- sequence [compileExpr (Var x) env fvs | x <- Environment.domain closure_env]
         return $ CFunCall fun_name args
compileExpr (App e1 e2) env fvs
    = do v1 <- Analysis.lookup e1 env <$> analysis
         v2 <- Analysis.lookup e2 env <$> analysis
         fun_name <- getAppName v1 v2
         case v1 of
           AbstractScalar (Primitive _)
             -> do c2 <- compileExpr e2 env fvs
                   return $ CFunCall fun_name [c2]
           _ -> do c1 <- compileExpr e1 env fvs
                   c2 <- compileExpr e2 env fvs
                   return $ CFunCall fun_name [c1, c2]
compileExpr e@(Pair e1 e2) env fvs
    = do pair <- Analysis.lookup e env <$> analysis
         fun_name <- getConName pair
         c1 <- compileExpr e1 env fvs
         c2 <- compileExpr e2 env fvs
         return $ CFunCall fun_name [c1, c2]
compileExpr (Letrec bindings body) env fvs
    = compileExpr (pushLetrec bindings body) env fvs

-- Closure applications
type ClosureValuePair = (AbstractEnvironment, Name, CoreExpr, AbstractValue)

compileClosureApplication :: ClosureValuePair -> CG (CDecl, CDecl)
compileClosureApplication (env, x, e, v)
    = do ret_type <- typeOf =<< (refineApply closure v <$> analysis)
         fun_name <- getAppName closure v
         formals  <- sequence [ liftM2 (,) (typeOf val) (return var)
                              | (val, var) <- [(closure, "c"), (v, zencode x)]
                              ]
         ret_stat <- CReturn <$> compileExpr e (Environment.insert x v env) (Environment.domain env)
         let proto = CFunProto ret_type fun_name formals
         return (CFunProtoDecl proto, CFunDecl proto [ret_stat])
    where
      closure = AbstractClosure env x e

enumClosureValuePairs :: AbstractAnalysis -> [ClosureValuePair]
enumClosureValuePairs a
    = nub [ (env, x, e, v)
          | c@(AbstractClosure env x e) <- Analysis.values a
          , v <- Analysis.values a
          , refineApply c v a /= AbstractBottom
          ]

compileClosureApplications :: CG [(CDecl, CDecl)]
compileClosureApplications
    = (enumClosureValuePairs <$> analysis) >>= mapM compileClosureApplication

-- Primitive applications
type PrimitiveValuePair = (Primitive, AbstractValue)

compilePrimitiveApplication :: PrimitiveValuePair -> CG (CDecl, CDecl)
compilePrimitiveApplication (p, v)
    = do ret_type <- typeOf =<< (refineApply primitive v <$> analysis)
         fun_name <- getAppName primitive v
         formal   <- liftM2 (,) (typeOf v) (return "x")
         ret_stat <- CReturn <$> compilePrimitive p v (CVar "x")
         let proto = CFunProto ret_type fun_name [formal]
         return (CFunProtoDecl proto, CFunDecl proto [ret_stat])
    where
      primitive = AbstractScalar (Primitive p)

enumPrimitiveValuePairs :: AbstractAnalysis -> [PrimitiveValuePair]
enumPrimitiveValuePairs a
    = nub [ (p, v)
          | (App operator operand, env) <- Analysis.domain a
          , AbstractScalar (Primitive p) <- [Analysis.lookup operator env a]
          , let v = Analysis.lookup operand  env a
          ]

compilePrimitiveApplications :: CG [(CDecl, CDecl)]
compilePrimitiveApplications
    = (enumPrimitiveValuePairs <$> analysis) >>= mapM compilePrimitiveApplication

compilePrimitive :: Primitive -> AbstractValue -> CExpr -> CG CExpr
compilePrimitive Car       = compileCar
compilePrimitive Cdr       = compileCdr
compilePrimitive Add       = compileArithmetic (+)  "+"
compilePrimitive Sub       = compileArithmetic (-)  "-"
compilePrimitive Mul       = compileArithmetic (*)  "*"
compilePrimitive Div       = compileArithmetic (/)  "/"
compilePrimitive Eql       = compileComparison (==) "=="
compilePrimitive Neq       = compileComparison (/=) "!="
compilePrimitive LTh       = compileComparison (<)  "<"
compilePrimitive LEq       = compileComparison (<=) "<="
compilePrimitive GTh       = compileComparison (>)  ">"
compilePrimitive GEq       = compileComparison (>=) ">="
compilePrimitive Neg       = compileNeg
compilePrimitive Exp       = compileUnary exp   "exp"
compilePrimitive Log       = compileUnary log   "log"
compilePrimitive Sin       = compileUnary sin   "sin"
compilePrimitive Cos       = compileUnary cos   "cos"
compilePrimitive Tan       = compileUnary tan   "tan"
compilePrimitive Asin      = compileUnary asin  "asin"
compilePrimitive Acos      = compileUnary acos  "acos"
compilePrimitive Atan      = compileUnary atan  "atan"
compilePrimitive Sinh      = compileUnary sinh  "sinh"
compilePrimitive Cosh      = compileUnary cosh  "cosh"
compilePrimitive Tanh      = compileUnary tanh  "tanh"
compilePrimitive Sqrt      = compileUnary sqrt  "sqrt"
compilePrimitive Asinh     = compileUnary asinh "asinh"
compilePrimitive Acosh     = compileUnary acosh "acosh"
compilePrimitive Atanh     = compileUnary atanh "atanh"
compilePrimitive Pow       = compilePow
compilePrimitive IfProc    = compileIfProc
compilePrimitive RealPrim  = compileRealPrim
compilePrimitive IsNull    = compileIsNull
compilePrimitive IsPair    = compileIsPair
compilePrimitive IsReal    = compileIsReal
compilePrimitive IsBoolean = compileIsBoolean

compileCar :: AbstractValue -> CExpr -> CG CExpr
compileCar _ x = return $ car x

compileCdr :: AbstractValue -> CExpr -> CG CExpr
compileCdr _ x = return $ cdr x

compileArithmetic :: (Float -> Float -> Float)
                  -> Name
                  -> AbstractValue
                  -> CExpr
                  -> CG CExpr
compileArithmetic op _ (AbstractPair (AbstractScalar (Real r1))
                                     (AbstractScalar (Real r2))) _
    = return $ CDoubleLit (r1 `op` r2)
compileArithmetic _ op_name v x
    = liftM2 (CBinaryOp op_name) (compileCar v x) (compileCdr v x)

compileComparison :: (Float -> Float -> Bool)
                  -> Name
                  -> AbstractValue
                  -> CExpr
                  -> CG CExpr
compileComparison op _ (AbstractPair (AbstractScalar (Real r1))
                                     (AbstractScalar (Real r2))) _
    = return . bool2int $ r1 `op` r2
    where
      bool2int True  = ctrue
      bool2int False = cfalse
compileComparison _ op_name v x
    = liftM2 (CBinaryOp op_name) (compileCar v x) (compileCdr v x)

compileNeg :: AbstractValue -> CExpr -> CG CExpr
compileNeg (AbstractScalar (Real r)) _
    = return $ CDoubleLit (negate r)
compileNeg _ x
    = return $ CNegate x

compileUnary :: (Float -> Float)
             -> Name
             -> AbstractValue
             -> CExpr
             -> CG CExpr
compileUnary fun _ (AbstractScalar (Real r)) _
    = return $ CDoubleLit (fun r)
compileUnary _ fun_name _ x
    = return $ CFunCall fun_name [x]

compilePow :: AbstractValue -> CExpr -> CG CExpr
compilePow (AbstractPair (AbstractScalar (Real r1))
                  (AbstractScalar (Real r2))) _
    = return $ CDoubleLit (r1 ** r2)
compilePow v x = (CFunCall "pow") <$> sequence [compileCar v x, compileCdr v x]

compileIfProc :: AbstractValue -> CExpr -> CG CExpr
compileIfProc (AbstractPair (AbstractScalar (Boolean True))
                            (AbstractPair thunk _)) x
    = compileIfBranch thunk (cadr x)
compileIfProc (AbstractPair (AbstractScalar (Boolean False))
                            (AbstractPair _ thunk)) x
    = compileIfBranch thunk (cddr x)
compileIfProc (AbstractPair AbstractBoolean
                            (AbstractPair thunk1 thunk2)) x
    = liftM2 (CTernaryCond (car x))
             (compileIfBranch thunk1 (cadr x))
             (compileIfBranch thunk2 (cddr x))
compileIfProc v _
    = error $ "compileIfProc: malformed IF expression: " ++ show v

compileIfBranch :: AbstractValue -> CExpr -> CG CExpr
compileIfBranch thunk expr
    = do fun_name <- getAppName thunk (AbstractScalar Nil)
         con_name <- getConName (AbstractScalar Nil)
         return $ CFunCall fun_name [expr, CFunCall con_name []]

compileRealPrim :: AbstractValue -> CExpr -> CG CExpr
compileRealPrim (AbstractScalar (Real r)) _ = return $ CDoubleLit r
compileRealPrim _                         x = return x

car, cdr, cadr, cddr :: CExpr -> CExpr
car x = CSlotAccess x "a"
cdr x = CSlotAccess x "d"
cadr  = car . cdr
cddr  = cdr . cdr

compileIsNull :: AbstractValue -> CExpr -> CG CExpr
compileIsNull (AbstractScalar Nil)            _ = return ctrue
compileIsNull _                               _ = return cfalse

compileIsPair :: AbstractValue -> CExpr -> CG CExpr
compileIsPair (AbstractPair _ _)              _ = return ctrue
compileIsPair _                               _ = return cfalse

compileIsReal :: AbstractValue -> CExpr -> CG CExpr
compileIsReal (AbstractScalar (Real _))       _ = return ctrue
compileIsReal AbstractReal                    _ = return ctrue
compileIsReal _                               _ = return cfalse

compileIsBoolean :: AbstractValue -> CExpr -> CG CExpr
compileIsBoolean (AbstractScalar (Boolean _)) _ = return ctrue
compileIsBoolean AbstractBoolean              _ = return ctrue
compileIsBoolean _                            _ = return cfalse

-- Compile main function
compileMain :: CoreExpr -> AbstractEnvironment -> CG CDecl
compileMain expression environment
    = do res      <- Analysis.lookup expression environment <$> analysis
         res_type <- typeOf res
         res_expr <- compileExpr expression environment []
         let proto = CFunProto CInt "main" []
             body  = [ CLocalVarDecl res_type "result" res_expr
                     -- If possible, print the result to the stdout.
                     , case res_type of
                         CInt    -> CPrintf "%d\\n" [CVar "result"]
                         CDouble -> CPrintf "%f\\n" [CVar "result"]
                         _       -> CPrintf "Can't display structs" []
                     , CReturn (CIntLit 0)
                     ]
         return $ CFunDecl proto body

compileProg :: (CoreExpr, ScalarEnvironment) -> CProg
compileProg program@(expression, initialEnvironment)
    = runCG code (analyze program)
    where
      environment = abstractEnvironment initialEnvironment
      code        = do structs    <- compileStructDecls
                       closures   <- compileClosureApplications
                       primitives <- compilePrimitiveApplications
                       globalVars <- compileGlobalVarDecls environment
                       entryPoint <- compileMain expression environment
                       let (struct_decls,     struct_cons    ) = unzip structs
                           (closure_protos,   closure_defns  ) = unzip closures
                           (primitive_protos, primitive_defns) = unzip primitives
                           includes = [CInclude "<stdio.h>", CInclude "<math.h>"]
                           protos   = closure_protos ++ primitive_protos
                           defns    = closure_defns  ++ primitive_defns
                           decls    = concat [ includes
                                             , sortBy (comparing strIndex)  struct_decls
                                             , struct_cons
                                             , protos
                                             , globalVars
                                             , [entryPoint]
                                             , defns
                                             ]
                       return decls
      strIndex (CStructDecl _ _ i) = i

compile :: String -> String
compile = emitProg . compileProg . read
