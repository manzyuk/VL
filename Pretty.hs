{-# LANGUAGE TypeSynonymInstances, TypeOperators #-}
module VL.Pretty (pprint) where

import VL.Common
import VL.Scalar
import VL.Coproduct
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

pprint :: Pretty a => a -> String
pprint = render . pp

instance Pretty Name where
    pp = text

-- Pretty-printing of expressions
class Functor f => Display f where
    displayAlg :: f Doc -> Doc

instance Display f => Pretty (Expr f) where
    pp = foldExpr displayAlg

instance Display Variable where
    displayAlg (Variable x) = text x

instance Display LambdaOneArg where
    displayAlg (LambdaOneArg arg body)
        = parens $ hang (text "lambda" <+> parens (text arg)) 1 body

instance Display LambdaManyArgs where
    displayAlg (LambdaManyArgs args body)
        = parens $ hang (text "lambda" <+> parens (sepMap text args)) 1 body

instance Display ApplicationOneArg where
    displayAlg (ApplicationOneArg operator operand)
        = parens $ sep [operator, operand]

instance Display ApplicationManyArgs where
    displayAlg (ApplicationManyArgs operator operands)
        = parens $ sep (operator : operands)

instance Display Cons where
    displayAlg (Cons x1 x2)
        = parens $ sep [text "cons", x1, x2]

instance Display List where
    displayAlg (List xs)
        = parens $ sep (text "list" : xs)

instance Display ConsStar where
    displayAlg (ConsStar xs)
        = parens $ sep (text "cons*" : xs)

instance Display If where
    displayAlg (If predicate consequent alternate)
        = parens $ sep [text "if", predicate, consequent, alternate]

instance Display Or where
    displayAlg (Or xs)
        = parens $ sep (text "or" : xs)

instance Display And where
    displayAlg (And xs)
        = parens $ sep (text "and" : xs)

instance Display Not where
    displayAlg (Not x)
        = parens $ sep [text "not", x]

instance Display Cond where
    displayAlg (Cond clauses)
        = parens $ sep (text "cond" : map ppClause clauses)
        where
          ppClause (test, expression) = parens $ sep [test, expression]

instance Display Let where
    displayAlg (Let bindings body)
        = parens $ sep [ text "let"
                       , parens . sepMap ppBinding $ bindings
                       , body
                       ]
        where
          ppBinding (name, expression)
              = parens $ sep [text name, expression]

instance Display LetrecOneArg where
    displayAlg (LetrecOneArg bindings body)
        = parens $ sep [ text "letrec"
                       , parens . sepMap ppBinding $ bindings
                       , body
                       ]
        where
          ppBinding (name, arg, body)
              = parens $ hang (text name <+> parens (text arg)) 1 body

instance Display LetrecManyArgs where
    displayAlg (LetrecManyArgs bindings body)
        = parens $ sep [ text "letrec"
                       , parens . sepMap ppBinding $ bindings
                       , body]
        where
          ppBinding (name, args, body)
              = parens $ hang (text name <+> parens (sepMap text args)) 1 body

sepMap :: (a -> Doc) -> [a] -> Doc
sepMap f = sep . map f

instance (Display f, Display g) => Display (f :+: g) where
    displayAlg (Inl x) = displayAlg x
    displayAlg (Inr x) = displayAlg x

-- Pretty-printing of scalars
instance Pretty Scalar where
    pp Nil             = parens empty
    pp (Boolean True)  = text "#t"
    pp (Boolean False) = text "#f"
    pp (Real r)        = float r
    pp (Primitive p)   = pp p

-- Pretty-printing of primitives
instance Pretty Primitive where
    pp Car       = prim "car"
    pp Cdr       = prim "cdr"
    pp Add       = prim "+"
    pp Sub       = prim "-"
    pp Mul       = prim "*"
    pp Div       = prim "/"
    pp Eql       = prim "=="
    pp Neq       = prim "/="
    pp LTh       = prim "<"
    pp LEq       = prim "<="
    pp GTh       = prim ">"
    pp GEq       = prim ">="
    pp Exp       = prim "exp"
    pp Log       = prim "log"
    pp Pow       = prim "**"
    pp Sin       = prim "sin"
    pp Cos       = prim "cos"
    pp Tan       = prim "tan"
    pp Sqrt      = prim "sqrt"
    pp Asin      = prim "asin"
    pp Acos      = prim "acos"
    pp Atan      = prim "atan"
    pp Sinh      = prim "sinh"
    pp Cosh      = prim "cosh"
    pp Tanh      = prim "tanh"
    pp Asinh     = prim "asinh"
    pp Acosh     = prim "acosh"
    pp Atanh     = prim "atanh"
    pp Neg       = prim "negate"
    pp IfProc    = prim "if-procedure"
    pp IsNull    = prim "null?"
    pp IsPair    = prim "pair?"
    pp IsReal    = prim "real?"
    pp IsBoolean = prim "boolean?"
    pp RealPrim  = prim "real"

-- Helper combinator for pretty-printing internal objects
internal :: String -> Doc -> Doc
internal name contents = text "#[" <> sep [text name, contents] <> char ']'

prim :: String -> Doc
prim = internal "primitive" . text

-- Pretty-printing of environments
instance Pretty val => Pretty (Environment val) where
    pp = parens . sep . map ppBinding . Environment.bindings
        where
          ppBinding (x, v) = ppPair x v

ppClosure :: Pretty val => Environment val -> Name -> CoreExpression -> Doc
ppClosure env x b = internal "closure" $ pp env $+$ pp (mkLambdaOneArg x b)

ppPair :: (Pretty a, Pretty b) => a -> b -> Doc
ppPair x y = parens $ sep [pp x, dot, pp y]

-- Pretty-printing of values
instance Pretty ConcreteValue where
    pp (ConcreteScalar s)        = pp s
    pp (ConcreteClosure env x b) = ppClosure env x b
    pp (ConcretePair v1 v2)      = ppPair v1 v2

instance Pretty AbstractValue where
    pp (AbstractScalar s)        = pp s
    pp AbstractBoolean           = text "B"
    pp AbstractReal              = text "R"
    pp AbstractBottom            = text "_|_"
    pp (AbstractClosure env x b) = ppClosure env x b
    pp (AbstractPair v1 v2)      = ppPair v1 v2

-- Pretty-printing of analyses
instance Pretty AbstractAnalysis where
    pp analysis = internal "analysis" bindings
        where
          bindings = vcat
                   . punctuate newline
                   . map ppBinding
                   . Analysis.toList
                   $ analysis
          ppBinding ((e, env), v) = sep [ ppPair e env
                                        , text "==>"
                                        , pp v
                                        ]

dot, newline :: Doc
dot     = char '.'
newline = char '\n'
