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

import VL.Pretty (pprint)

import VL.Uniquify
import VL.ZEncoding

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Arrow

data CType
    = CInt
    | CDouble
    | CStruct Name [(CType, Name)]
      deriving Show

data CExpr
    = CVariable Name
    | CSlotAccess Name Name
    | CFunctionCall Name [CExpr]
    | CReturn CExpr

data CDecl
    = CStructDecl Name [(CType, Name)]
    | CFunctionDecl CType Name [(CType, Name)] CExpr

type CProg = [CDecl]

isVoid :: AbstractValue -> Bool
isVoid (AbstractScalar _)        = True
isVoid AbstractReal              = False
isVoid AbstractBoolean           = False
isVoid (AbstractClosure env _ _) = all isVoid $ Environment.values env
isVoid (AbstractPair v1 v2)      = isVoid v1 && isVoid v2
isVoid AbstractBottom            = True

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
genCType :: (AbstractValue -> Name) -> (Name -> Name) -> AbstractValue -> CType
genCType _ _ v | isVoid v    = error "getCType: the argument is void"
genCType _ _ AbstractBoolean = CInt
genCType _ _ AbstractReal    = CDouble
genCType sFun xFun v@(AbstractClosure env _ _) = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, xFun x)
           | (x, v) <- Environment.bindings env
           , isNonVoid v
           ]
getCType sFun xFun v@(AbstractPair v1 v2) = CStruct (sFun v) bs
    where
      bs = [ (genCType sFun xFun v, xFun x)
           | (x, v) <- zip ["a", "d"] [v1, v2]
           , isNonVoid v
           ]

genCProg :: (CoreExpression, ScalarEnvironment) -> CProg
genCProg program@(expression, constants) = undefined
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
