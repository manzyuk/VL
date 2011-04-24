{-# LANGUAGE PatternGuards #-}
module VL.Compiler.CodeGenerator where

import VL.Language.Common
import VL.Language.Scalar
import VL.Language.Syntax
import VL.Language.Expression

import VL.Alacarte.Coproduct
import VL.Alacarte.FixedPoint

import VL.Language.Environment (Environment)
import qualified VL.Language.Environment as Environment

import VL.Abstract.Value

import VL.Abstract.Analysis (AbstractAnalysis)
import qualified VL.Abstract.Analysis as Analysis

import VL.Abstract.Evaluator hiding (float)

import VL.Language.Parser (parse)
import VL.Language.Pretty
import VL.Language.Prepare

import VL.Language.Uniquify
import VL.Compiler.ZEncoding

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Arrow (first, second)

import Text.PrettyPrint

import Debug.Trace

-- Types
data CType
    = CInt
    | CFloat
    | CStruct Name [(CType, Name)]
      deriving Show

getCTypeName :: CType -> Name
getCTypeName CInt             = "int"
getCTypeName CFloat           = "float"
getCTypeName (CStruct name _) = name

-- Expression
data CExpr
    = CVariable Name
    | CStructCon [CExpr]
    | CSlotAccess Name Name
    | CIntLit Int
    | CFloatLit Float
    | CFunctionCall Name [CExpr]
      deriving Show

-- Statements
data CStat
    = CReturn CExpr
    | CVariableDecl CType Name CExpr
      deriving Show

-- Top-level declarations
data CDecl
    = CGlobalDecl CType Name CExpr
    | CStructDecl Name [(CType, Name)]
    | CFunctionDecl CType Name [(CType, Name)] [CStat]
      deriving Show

type CProg = [CDecl]

-- Pretty-printing

-- Lay out items of a list in a row, pretty-printing each item with
-- a supplied printer and separating by commas.
row :: (a -> Doc) -> [a] -> Doc
row printer = hsep . punctuate comma . map printer

-- Lay out items of a list in a column, pretty-printing each with a
-- supplied printer.
col :: (a -> Doc) -> [a] -> Doc
col printer = vcat . map printer

ppCProg :: CProg -> Doc
ppCProg = col ppCDecl

ppCDecl :: CDecl -> Doc
ppCDecl (CGlobalDecl ty var val)
    = text (getCTypeName ty) <+> text var <+> equals <+> ppCExpr val <> semi
ppCDecl (CStructDecl name slots)
    = vcat [ text "typedef struct" <+> lbrace
           , nest 4 (col ppSlot slots)
           , rbrace <+> text name <> semi
           ]
    where
      ppSlot (ty, x) = text (getCTypeName ty) <+> text x <> semi
ppCDecl (CFunctionDecl ty name formals stats)
    = vcat [ text (getCTypeName ty) <+> text name
                     <> parens (row ppFormal formals) <+> lbrace
           , nest 4 (col ppCStat stats)
           , rbrace <> semi
           ]
    where
      ppFormal (ty, name) = text (getCTypeName ty) <+> text name

ppCStat :: CStat -> Doc
ppCStat (CReturn e)
    = text "return" <+> ppCExpr e <> semi
ppCStat (CVariableDecl ty name value)
    = text (getCTypeName ty) <+> text name <+> equals <+> ppCExpr value <> semi

ppCExpr :: CExpr -> Doc
ppCExpr (CVariable x)             = text x
ppCExpr (CStructCon vs)           = braces (row ppCExpr vs)
ppCExpr (CSlotAccess x y)         = text x <> char '.' <> text y
ppCExpr (CIntLit i)               = int i
ppCExpr (CFloatLit f)             = float f
ppCExpr (CFunctionCall name args) = text name <> parens (row ppCExpr args)

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
-- a given abstract value, which is assumed to be non-void.
genCType :: (AbstractValue -> Name)
         -> (Name -> Name)
         -> AbstractValue
         -> CType
genCType _    _    (AbstractScalar (Boolean _))     = CInt
genCType _    _    AbstractBoolean                  = CInt
genCType _    _    (AbstractScalar (Real _))        = CFloat
genCType _    _    AbstractReal                     = CFloat
genCType sFun _    v@(AbstractScalar (Primitive _)) = CStruct (sFun v) []
genCType sFun _    v@(AbstractScalar Nil)           = CStruct (sFun v) []
genCType sFun xFun v@(AbstractClosure env _ _)      = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, xFun x)
           | (x, v) <- Environment.bindings env
           ]
genCType sFun xFun v@(AbstractPair v1 v2)           = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, x)
           | (x, v) <- zip ["a", "d"] [v1, v2]
           ]

genCStructDecl :: (AbstractValue -> CType)
               -> (AbstractValue -> Name)
               -> AbstractValue
               -> CProg
genCStructDecl tFun mFun v
    | t@(CStruct name slots) <- tFun v
    = let formals = slots
          body    = [ CVariableDecl t "temp" (CStructCon . map (CVariable . snd) $ formals)
                    , CReturn (CVariable "temp")
                    ]
      in [ CStructDecl name slots
         , CFunctionDecl t (mFun v) formals body
         ]
    | otherwise = []

genCValue :: (AbstractValue -> Name) -> AbstractValue -> CExpr
genCValue mFun v@(AbstractScalar Nil)           = CFunctionCall (mFun v) []
genCValue _    (AbstractScalar (Boolean True))  = CIntLit 1
genCValue _    (AbstractScalar (Boolean False)) = CIntLit 0
genCValue _    (AbstractScalar (Real r))        = CFloatLit r
genCValue mFun v@(AbstractScalar (Primitive p)) = CFunctionCall (mFun v) []
genCValue mFun v@(AbstractClosure env x e)      = CFunctionCall name args
    where
      name = mFun v
      args = map (genCValue mFun) (Environment.values env)
genCValue mFun v@(AbstractPair v1 v2)           = CFunctionCall name args
    where
      name = mFun v
      args = [genCValue mFun v1, genCValue mFun v2]
genCValue _    AbstractBoolean = error "genCValue: AbstractBoolean"
genCValue _    AbstractReal    = error "genCValue: AbstractReal"
genCValue _    AbstractBottom  = error "genCValue: AbstractBottom"

genCGlobalDecl :: (AbstractValue -> CType)
               -> (AbstractValue -> Name)
               -> Name
               -> AbstractValue
               -> CDecl
genCGlobalDecl tFun mFun x v = CGlobalDecl (tFun v) x (genCValue mFun v)

genCExpr :: (Name -> Name)                           -- X
         -> (AbstractValue -> Name)                  -- M
         -> ((AbstractValue, AbstractValue) -> Name) -- F
         -> AbstractAnalysis                         -- a*
         -> AbstractEnvironment
         -> CoreExpr
         -> CExpr
genCExpr xFun mFun fFun a env (Var x)
    | x `elem` (Environment.domain env) -- x is bound in env
    = CVariable (xFun x)
    | otherwise
    = CSlotAccess "c" (xFun x)
genCExpr xFun mFun fFun a env e@(Lam _ _)
    = CFunctionCall name args
    where
      name = mFun $ Analysis.lookup e env a
      args = [ CVariable x | (x, v) <- Environment.bindings env ]
genCExpr xFun mFun fFun a env (App e1 e2)
    = CFunctionCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = fFun (v1, v2)
      args = [ genCExpr xFun mFun fFun a env o
             | (o, v) <- [(e1, v1), (e2, v2)]
             ]
genCExpr xFun mFun fFun a env (Pair e1 e2)
    = CFunctionCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = mFun $ AbstractPair v1 v2
      args = [ genCExpr xFun mFun fFun a env e
             | (e, v) <- [(e1, v1), (e2, v2)]
             ]
genCExpr xFun mFun fFun a env (Letrec bindings body)
    = error "genCExpr: LETREC is not supported yet"

genFunctionDecl :: (Name -> Name)                           -- X
                -> (AbstractValue -> CType)                 -- T
                -> (AbstractValue -> Name)                  -- M
                -> ((AbstractValue, AbstractValue) -> Name) -- F
                -> AbstractAnalysis                         -- a*
                -> AbstractValue
                -> AbstractEnvironment
                -> Name
                -> CoreExpr
                -> CDecl
genFunctionDecl xFun tFun mFun fFun a v env x e
    = CFunctionDecl ty name formals body
    where
      c       = AbstractClosure env x e
      ty      = tFun $ refineApply c v a
      name    = fFun (c, v)
      formals = [ (tFun v, p) | (v, p) <- [(c, "c"), (v, xFun x)] ]
      body    = [ CReturn $ genCExpr xFun mFun fFun a (Environment.insert x v env) e ]

genCProg :: (CoreExpr, ScalarEnvironment) -> CProg
genCProg program@(expression, constants) = structDecls ++ functionDecls ++ globals ++ [entryPoint]
    where
      analysis = analyze program
      values   = Analysis.values analysis

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

      closureValuePairs = [ (env', x, b, v)
                          | (App l@(Lam x b) e, env) <- Analysis.domain analysis
                          , let env' = Environment.restrict (freeVariables l) env
                                v    = Analysis.lookup e env analysis
                          ]

      structDecls   = concatMap (genCStructDecl tFun mFun) values
      functionDecls = [ genFunctionDecl xFun tFun mFun fFun analysis v env x e
                      | (env, x, e, v) <- closureValuePairs
                      ]
      entryPoint    = CFunctionDecl CInt "main" [] [ CVariableDecl (tFun result) "result" (genCExpr xFun mFun fFun analysis environment expression)
                                                   , CReturn (CIntLit 0)
                                                   ]
      globals       = [ genCGlobalDecl tFun mFun (zencode x) v
                      | (x, v) <- Environment.bindings . Environment.restrict (freeVariables expression) $ environment
                      ]
      environment   = initialAbstractEnvironment constants
      result        = Analysis.lookup expression environment analysis
