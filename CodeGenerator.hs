{-# LANGUAGE PatternGuards #-}
module VL.CodeGenerator where

import VL.Common
import VL.Scalar
import VL.Expression

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.AbstractValue

import VL.AbstractAnalysis (AbstractAnalysis)
import qualified VL.AbstractAnalysis as Analysis

import VL.AbstractEvaluator

import VL.Parser (parse)
import VL.Pretty
import VL.Prepare

import VL.Uniquify
import VL.ZEncoding

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Arrow (first, second)

import Text.PrettyPrint

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
mkLookupFun :: Ord k => Map k v -> (k -> v)
mkLookupFun table key = fromMaybe (error msg) (Map.lookup key table)
    where
      msg = "mkLookupFun: key is not found"

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

genCProg :: (CoreExpression, ScalarEnvironment) -> CProg
genCProg program@(expression, constants) = structDecls
    where
      analysis = analyze program
      values   = Analysis.values analysis
      nonVoids = filter isNonVoid values

      xTbl = Map.fromList [(n, zencode n) | n <- Set.toList (variables expression)]
      xFun = mkLookupFun xTbl
      sTbl = mkLookupTbl "#:val-" values
      sFun = mkLookupFun sTbl
      fTbl = mkLookupTbl "#:fun-" [ (v1, v2) | v1 <- values, v2 <- values ]
      fFun = mkLookupFun fTbl
      tTbl = Map.fromList [(v, genCType sFun xFun v) | v <- nonVoids ]
      tFun = mkLookupFun tTbl
      mTbl = mkLookupTbl "#:con-" nonVoids
      mFun = mkLookupFun mTbl

      structDecls = concatMap (genCStructDecl tFun mFun) nonVoids
