{-# LANGUAGE TypeSynonymInstances #-}
module VL.Pretty
    ( Pretty
    , pp
    , render
    )
    where

import VL.Common
import VL.Scalar
import VL.Expression
import VL.Environment (Environment)
import qualified VL.Environment as Environment (bindings)

import VL.ConcreteValue (ConcreteValue (..))
import VL.AbstractValue (AbstractValue (..))

import VL.AbstractAnalysis (AbstractAnalysis)
import qualified VL.AbstractAnalysis as Analysis (toList)

import Text.PrettyPrint

class Pretty a where
    pp :: a -> Doc

instance Pretty Name where
    pp = text

instance Pretty binder => Pretty (Expression binder) where
    pp (Variable x) = pp x
    pp (Lambda x b)
        = parens $ hang (text "lambda" <+> parens (pp x)) 1 (pp b)
    pp (Application e1 e2)
        = parens $ sep [pp e1, pp e2]
    pp (Cons e1 e2)
        = parens $ sep [text "cons", pp e1, pp e2]

instance Pretty Scalar where
    pp Nil             = parens empty
    pp (Boolean True)  = text "#t"
    pp (Boolean False) = text "#f"
    pp (Real r)        = float r
    pp (Primitive p)   = pp p

instance Pretty Primitive where
    pp Car = text "car"
    pp Cdr = text "cdr"

instance Pretty val => Pretty (Environment val) where
    pp = brackets . sep . map ppBinding . Environment.bindings
        where
          ppBinding (x, v) = parens $ text x <+> char '.' <+> (pp v)

ppClosure :: Pretty val => Environment val -> Name -> CoreExpression -> Doc
ppClosure env x b = text "#<" <> vcat [ text "closure:"
                                      , pp env
                                      , pp (Lambda x b)
                                      ]
                              <> char '>'

ppPair :: (Pretty a, Pretty b) => a -> b -> Doc
ppPair x y = parens (pp x <> comma <+> pp y)

instance Pretty ConcreteValue where
    pp (ConcreteScalar s)        = pp s
    pp (ConcreteClosure env x b) = ppClosure env x b
    pp (ConcretePair v1 v2)      = ppPair v1 v2

instance Pretty AbstractValue where
    pp (AbstractScalar s)        = pp s
    pp AbstractBoolean           = text "B"
    pp AbstractReal              = text "R"
    pp AbstractTop               = text "T"
    pp (AbstractClosure env x b) = ppClosure env x b
    pp (AbstractPair v1 v2)      = ppPair v1 v2

instance Pretty AbstractAnalysis where
    pp analysis = text "#<" <> vcat [ text "ananlysis:"
                                    , bindings
                                    ]
                            <> char '>'
        where
          bindings = vcat . map ppBinding . Analysis.toList $ analysis
          ppBinding ((e, env), v) = sep [ ppPair e env
                                        , text "==>"
                                        , pp v
                                        ]