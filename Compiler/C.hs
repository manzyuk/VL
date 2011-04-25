module VL.Compiler.C where

import VL.Language.Common
import VL.Language.Pretty

-- Types
data CType
    = CInt
    | CDouble
    | CStruct Name [(CType, Name)]
      deriving Show

-- Expressions
data CExpr
    = CVar Name
    | CStructCon [CExpr]
    | CSlotAccess CExpr Name
    | CIntLit Int
    -- TODO: Change Float to Double in the rest of the
    -- code and get rid of the following discrepancy.
    | CDoubleLit Float
    | CFunCall Name [CExpr]
    | CBinaryOp Name CExpr CExpr
    | CTernaryCond CExpr CExpr CExpr
      deriving Show

-- Statements
data CStat
    = CReturn CExpr
    | CLocalVarDecl CType Name CExpr
      deriving Show

-- Top-level declarations
--
-- Function prototypes are necessary because in the generated code
-- functions can be mutually recursive.  Also, prototypes free us
-- from the burden of topologically sorting function definitions.
-- We'll still have to topologically sort 'struct' definitions.
data CDecl
    = CFunProtoDecl CFunProto
    | CFunDecl CFunProto [CStat]
    | CGlobalVarDecl CType Name CExpr
    | CStructDecl Name [(CType, Name)]
      deriving Show

data CFunProto
    = CFunProto CType Name [(CType, Name)]
      deriving Show

-- A C program is a list of top-level declarations.
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

instance Pretty CType where
    pp CInt             = text "int"
    pp CDouble          = text "double"
    pp (CStruct name _) = text name

instance Pretty CExpr where
    pp (CVar x)                = text x
    pp (CStructCon slots)      = braces (row pp slots)
    pp (CSlotAccess expr slot) = pp expr <> dot <> text slot
    pp (CIntLit i)             = int i
    pp (CDoubleLit f)          = float f
    pp (CFunCall name args)    = text name <> parens (row pp args)
    pp (CBinaryOp op x y)      = parens (pp x <+> text op <+> pp y)
    pp (CTernaryCond a b c)    = sep [ pp a
                                     , char '?'
                                     , pp b
                                     , colon
                                     , pp c
                                     ]

instance Pretty CStat where
    pp (CReturn e)
        = text "return" <+> pp e <> semi
    pp (CLocalVarDecl typ var val)
        = pp typ <+> text var <+> equals <+> pp val <> semi

instance Pretty CDecl where
    pp (CFunProtoDecl proto)
        = pp proto <> semi
    pp (CFunDecl proto body)
        = vcat [ pp proto <+> lbrace
               , nest 4 (col pp body)
               , rbrace <> semi
               ]
    pp (CGlobalVarDecl typ var val)
        = pp typ <+> text var <+> equals <+> pp val <> semi
    pp (CStructDecl name slots)
        = vcat [ text "typedef struct" <+> lbrace
               , nest 4 (col ppSlot slots)
               , rbrace <+> text name <> semi
               ]
        where
          ppSlot (typ, var) = pp typ <+> text var <> semi

instance Pretty CFunProto where
    pp (CFunProto ret_type fun_name formals)
        = pp ret_type <+> text fun_name <> parens (row ppFormal formals)
        where
          ppFormal (typ, var) = pp typ <+> text var

emitCProg :: CProg -> String
emitCProg = render . col pp
