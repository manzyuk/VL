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

import VL.Abstract.Evaluator

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
    | CDouble
    | CStruct Name [(CType, Name)]
      deriving Show

getCTypeName :: CType -> Name
getCTypeName CInt             = "int"
getCTypeName CDouble          = "double"
getCTypeName (CStruct name _) = name

-- Expression
data CExpr
    = CVariable Name
    | CStructCon [CExpr]
    | CSlotAccess Name Name
    | CIntLiteral Int
    | CFunctionCall Name [CExpr]
      deriving Show

-- Statements
data CStat
    = CReturn CExpr
    | CVariableDecl CType Name CExpr
      deriving Show

-- Top-level declarations
data CDecl
    = CStructDecl Name [(CType, Name)]
    | CFunctionDecl CType Name [(CType, Name)] [CStat]
      deriving Show

type CProg = [CDecl]

-- Pretty-printing

-- Lay out items of a list in a raw, pretty-printing each item with
-- a supplied printer and separating by commas.
raw :: (a -> Doc) -> [a] -> Doc
raw printer = hsep . punctuate comma . map printer

-- Lay out items of a list in a column, pretty-printing each with a
-- supplied printer.
col :: (a -> Doc) -> [a] -> Doc
col printer = vcat . map printer

ppCProg :: CProg -> Doc
ppCProg = col ppCDecl

ppCDecl :: CDecl -> Doc
ppCDecl (CStructDecl name slots)
    = vcat [ text "typedef struct" <+> lbrace
           , nest 4 (col ppSlot slots)
           , rbrace <+> text name <> semi
           ]
    where
      ppSlot (ty, x) = text (getCTypeName ty) <+> text x <> semi
ppCDecl (CFunctionDecl ty name formals stats)
    = vcat [ text (getCTypeName ty) <+> text name
                     <> parens (raw ppFormal formals) <+> lbrace
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
ppCExpr (CStructCon vs)           = braces (raw ppCExpr vs)
ppCExpr (CSlotAccess x y)         = text x <> char '.' <> text y
ppCExpr (CIntLiteral i)           = int i
ppCExpr (CFunctionCall name args) = text name <> parens (raw ppCExpr args)

-- Code generation
isVoid :: AbstractValue -> Bool
isVoid (AbstractScalar _)        = True
isVoid AbstractReal              = False
isVoid AbstractBoolean           = False
isVoid (AbstractClosure env _ _) = all isVoid $ Environment.values env
isVoid (AbstractPair v1 v2)      = isVoid v1 && isVoid v2
isVoid AbstractBottom            = error "isVoid: AbstractBottom"

isNonVoid :: AbstractValue -> Bool
isNonVoid = not . isVoid

-- Given a lookup /table/, return a lookup /function/.  The lookup
-- function signals an error when the key is not found.
mkLookupFun :: (Ord k, Show k) => Map k v -> (k -> v)
mkLookupFun table key = fromMaybe (error msg) (Map.lookup key table)
    where
      msg = "mkLookupFun: key is not found " ++ show key

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
genCType _ _ v | isVoid v    = error "getCType: the argument is void"
genCType _ _ AbstractBoolean = CInt
genCType _ _ AbstractReal    = CDouble
genCType sFun xFun v@(AbstractClosure env _ _) = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, xFun x)
           | (x, v) <- Environment.bindings env
           , isNonVoid v
           ]
genCType sFun xFun v@(AbstractPair v1 v2) = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, x)
           | (x, v) <- zip ["a", "d"] [v1, v2]
           , isNonVoid v
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
      args = [ CVariable x | (x, v) <- Environment.bindings env, isNonVoid v ]
genCExpr xFun mFun fFun a env (App e1 e2)
    = CFunctionCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = fFun (v1, v2)
      args = [ genCExpr xFun mFun fFun a env o
             | (o, v) <- [(e1, v1), (e2, v2)]
             , isNonVoid v
             ]
genCExpr xFun mFun fFun a env (Pair e1 e2)
    = CFunctionCall name args
    where
      v1   = Analysis.lookup e1 env a
      v2   = Analysis.lookup e2 env a
      name = mFun $ AbstractPair v1 v2
      args = [ genCExpr xFun mFun fFun a env e
             | (e, v) <- [(e1, v1), (e2, v2)]
             , isNonVoid v
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
      formals = [ (tFun v, p) | (v, p) <- [(c, "c"), (v, xFun x)], isNonVoid v ]
      body    = [ CReturn $ genCExpr xFun mFun fFun a (Environment.insert x v env) e ]

genCProg :: (CoreExpr, ScalarEnvironment) -> CProg
genCProg program@(expression, constants) = structDecls ++ functionDecls ++ [entryPoint]
    where
      analysis = analyze program
      values   = Analysis.values analysis
      nonVoids = filter isNonVoid values

      -- Build lookup tables and lookup functions for relevant data
      xTbl = Map.fromList [(n, zencode n) | n <- Set.toList (variables expression)]
      xFun = mkLookupFun xTbl   -- X from the paper
      sTbl = mkLookupTbl "#:val-" values
      sFun = mkLookupFun sTbl   -- S from the paper
      fTbl = mkLookupTbl "#:fun-" [ (v1, v2) | v1 <- values, v2 <- values ]
      fFun = mkLookupFun fTbl   -- F from the paper
      tTbl = Map.fromList [(v, genCType sFun xFun v) | v <- nonVoids ]
      tFun = mkLookupFun tTbl   -- T from the paper
      mTbl = mkLookupTbl "#:con-" nonVoids
      mFun = mkLookupFun mTbl   -- M from the paper

      closureValuePairs = [ (env', x, b, v)
                          | ((App l@(Lam x b) e, env), v) <- Analysis.toList analysis
                          , isNonVoid v
                          , let env' = Environment.restrict (freeVariables l) env
                          ]

      structDecls   = concatMap (genCStructDecl tFun mFun) nonVoids
      functionDecls = [ genFunctionDecl xFun tFun mFun fFun analysis v env x e
                      | (env, x, e, v) <- closureValuePairs
                      ]
      entryPoint    = CFunctionDecl CInt "main" [] [ CVariableDecl (tFun result) "result" (genCExpr xFun mFun fFun analysis environment expression)
                                                   , CReturn (CIntLiteral 0)
                                                   ]
      environment   = initialAbstractEnvironment constants
      result        = Analysis.lookup expression environment analysis
