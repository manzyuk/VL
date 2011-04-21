{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor              #-}
{-# LANGUAGE TemplateHaskell                                #-}
module VL.Language.Syntax where

import VL.Language.Common
import VL.Language.Pretty

import VL.Alacarte.Coproduct
import VL.Alacarte.FixedPoint

type Syntax = Fix

data Variable            a = Variable Name
			     deriving (Eq, Ord, Functor)

-- Lambda expressions
data LambdaOneArg        a = LambdaOneArg Name a
			     deriving (Eq, Ord, Functor)
data LambdaManyArgs      a = LambdaManyArgs [Name] a
			     deriving (Eq, Ord, Functor)

-- Procedure calls
data ApplicationOneArg   a = ApplicationOneArg a a
			     deriving (Eq, Ord, Functor)
data ApplicationManyArgs a = ApplicationManyArgs a [a]
			     deriving (Eq, Ord, Functor)

-- Pairs
data Cons                a = Cons a a
			     deriving (Eq, Ord, Functor)
data List                a = List [a]
			     deriving (Eq, Ord, Functor)
data ConsStar            a = ConsStar [a]
			     deriving (Eq, Ord, Functor)

-- Conditionals
data If                  a = If a a a
			     deriving (Eq, Ord, Functor)
data Or                  a = Or [a]
			     deriving (Eq, Ord, Functor)
data And                 a = And [a]
			     deriving (Eq, Ord, Functor)
data Not                 a = Not a
			     deriving (Eq, Ord, Functor)
data Cond                a = Cond [(a, a)]
			     deriving (Eq, Ord, Functor)

-- Binding constructs
data Let                 a = Let [(Name, a)] a
			     deriving (Eq, Ord, Functor)
data LetrecOneArg        a = LetrecOneArg [(Name, Name, a)] a
			     deriving (Eq, Ord, Functor)
data LetrecManyArgs      a = LetrecManyArgs [(Name, [Name], a)] a
			     deriving (Eq, Ord, Functor)

-- Folding over a syntax
foldSyntax :: Functor f => (f a -> a) -> Syntax f -> a
foldSyntax = cata

-- Smart constructors
$(defineSmartConstructors [ ''Variable
			  , ''LambdaOneArg
			  , ''LambdaManyArgs
			  , ''ApplicationOneArg
			  , ''ApplicationManyArgs
			  , ''Cons
			  , ''List
			  , ''ConsStar
			  , ''If
			  , ''Or
			  , ''And
			  , ''Not
			  , ''Cond
			  , ''Let
			  , ''LetrecOneArg
			  , ''LetrecManyArgs
			  ])

-- Type synonyms for the most important syntax types
type Core
    =     Variable
      :+: LambdaOneArg
      :+: ApplicationOneArg
      :+: Cons
      :+: LetrecOneArg

type CoreSyntax = Syntax Core

type Surface
    =     Variable
      :+: LambdaManyArgs
      :+: ApplicationManyArgs
      :+: Cons
      :+: List
      :+: ConsStar
      :+: If
      :+: Or
      :+: And
      :+: Not
      :+: Cond
      :+: Let
      :+: LetrecManyArgs

type SurfaceSyntax = Syntax Surface

-- Pretty-printing of syntaxes
class Functor f => Display f where
    displayAlg :: f Doc -> Doc

instance Display f => Pretty (Syntax f) where
    pp = foldSyntax displayAlg

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
	= ppList [operator, operand]

instance Display ApplicationManyArgs where
    displayAlg (ApplicationManyArgs operator operands)
	= ppList (operator : operands)

instance Display Cons where
    displayAlg (Cons x1 x2)
	= ppList [text "cons", x1, x2]

instance Display List where
    displayAlg (List xs)
	= ppList (text "list" : xs)

instance Display ConsStar where
    displayAlg (ConsStar xs)
	= ppList (text "cons*" : xs)

instance Display If where
    displayAlg (If predicate consequent alternate)
	= ppList [text "if", predicate, consequent, alternate]

instance Display Or where
    displayAlg (Or xs)
	= ppList (text "or" : xs)

instance Display And where
    displayAlg (And xs)
	= ppList (text "and" : xs)

instance Display Not where
    displayAlg (Not x)
	= ppList [text "not", x]

instance Display Cond where
    displayAlg (Cond clauses)
	= ppList (text "cond" : map ppClause clauses)
	where
	  ppClause (test, expression) = ppList [test, expression]

instance Display Let where
    displayAlg (Let bindings body)
	= ppList [ text "let"
		 , parens . sepMap ppBinding $ bindings
		 , body
		 ]
	where
	  ppBinding (name, expression)
	      = ppList [text name, expression]

instance Display LetrecOneArg where
    displayAlg (LetrecOneArg bindings body)
	= ppList [ text "letrec"
		 , parens . sepMap ppBinding $ bindings
		 , body
		 ]
	where
	  ppBinding (name, arg, body)
	      = parens $ hang (text name <+> parens (text arg)) 1 body

instance Display LetrecManyArgs where
    displayAlg (LetrecManyArgs bindings body)
	= ppList [ text "letrec"
		 , parens . sepMap ppBinding $ bindings
		 , body]
	where
	  ppBinding (name, args, body)
	      = parens $ hang (text name <+> parens (sepMap text args)) 1 body

instance (Display f, Display g) => Display (f :+: g) where
    displayAlg (Inl x) = displayAlg x
    displayAlg (Inr x) = displayAlg x
