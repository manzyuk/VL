{-# LANGUAGE RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances, IncoherentInstances      #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances    #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances            #-}
module VL.Alacarte where

import VL.Common

import VL.Scalar (Scalar, ScalarEnvironment)
import qualified VL.Scalar as Scalar

import VL.Environment (Environment)
import qualified VL.Environment as Environment

import VL.Token (Token, scan)
import qualified VL.Token as Token

import VL.Parser (Parser, extract, identifier, keywords, emptyList, parens, special)

import Text.Parsec.Prim       hiding (many, (<|>), State, parse)
import Text.Parsec.String     hiding (Parser)
import Text.Parsec.Combinator (between)

import Control.Applicative
import Control.Monad.State
import Control.Arrow (first, second, (***))

data (f :+: g) a = Inl (f a) | Inr (g a)
infixr 6 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl (fmap f x)
    fmap f (Inr x) = Inr (fmap f x)

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

-- GHC wouldn't allow us to write the following instances in a more
-- natural style, e.g., f :<: f, or f :<: (f :+: g).
instance Functor f => (:<:) f f where
    inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => (:<:) f (h :+: g) where
    inj = Inr . inj

data Expr f = In { out :: f (Expr f) }

data Variable    a = Variable Name
data Lambda      a = Lambda [Name] a
data Application a = Application a [a]
data Cons        a = Cons a a
data List        a = List [a]
data ConsStar    a = ConsStar [a]
data If          a = If a a a
data Let         a = Let [(Name, a)] a
data Letrec      a = Letrec [(Name, [Name], a)] a
data Cond        a = Cond [(a, a)]

instance Functor Variable where
    fmap _ (Variable x) = Variable x

instance Functor Lambda where
    fmap f (Lambda binder body) = Lambda binder (f body)

instance Functor Application where
    fmap f (Application operator operands)
        = Application (f operator) (map f operands)

instance Functor Cons where
    fmap f (Cons x y) = Cons (f x) (f y)

instance Functor List where
    fmap f (List xs) = List (map f xs)

instance Functor ConsStar where
    fmap f (ConsStar xs) = ConsStar (map f xs)

instance Functor If where
    fmap f (If predicate consequent alternate)
        = If (f predicate) (f consequent) (f alternate)

instance Functor Let where
    fmap f (Let bindings body)
        = Let (map (second f) bindings) (f body)

instance Functor Letrec where
    fmap f (Letrec bindings body)
        = Letrec (map (third f) bindings) (f body)

instance Functor Cond where
    fmap f (Cond branches)
        = Cond (map (f *** f) branches)

third :: (c1 -> c2) -> (a, b, c1) -> (a, b, c2)
third f (x, y, z) = (x, y, f z)

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

iVariable :: (Variable :<: f) => Name -> Expr f
iVariable x = inject (Variable x)

iLambda :: (Lambda :<: f) => [Name] -> Expr f -> Expr f
iLambda xs body = inject (Lambda xs body)

iApplication :: (Application :<: f) => Expr f -> [Expr f] -> Expr f
iApplication operator operands = inject (Application operator operands)

iCons :: (Cons :<: f) => Expr f -> Expr f -> Expr f
iCons x y = inject (Cons x y)

iList :: (List :<: f) => [Expr f] -> Expr f
iList xs = inject (List xs)

iConsStar :: (ConsStar :<: f) => [Expr f] -> Expr f
iConsStar xs = inject (ConsStar xs)

iIf :: (If :<: f) => Expr f -> Expr f -> Expr f -> Expr f
iIf predicate consequent alternate = inject (If predicate consequent alternate)

iLet :: (Let :<: f) => [(Name, Expr f)] -> Expr f -> Expr f
iLet bindings body = inject (Let bindings body)

iLetrec :: (Letrec :<: f) => [(Name, [Name], Expr f)] -> Expr f -> Expr f
iLetrec bindings body = inject (Letrec bindings body)

iCond :: (Cond :<: f) => [(Expr f, Expr f)] -> Expr f
iCond branches = inject (Cond branches)

type Surface  =  Variable
             :+: Lambda
             :+: Application
             :+: Cons
             :+: List
             :+: ConsStar
             :+: If
             :+: Let
             :+: Letrec
             :+: Cond

type SurfaceExpr = Expr Surface

-- Parser

nil, true, false :: Name
nil   = "#:nil"
true  = "#:true"
false = "#:false"

pVariable :: Parser SurfaceExpr
pVariable = iVariable <$> identifier

pConstant :: Parser SurfaceExpr
pConstant = do s <- try emptyList <|> extract getConstant
               (env, i) <- get
               let x = case s of
                         Scalar.Nil -> nil
                         Scalar.Boolean True -> true
                         Scalar.Boolean False -> false
                         Scalar.Real _ -> "#:real-" ++ show i
               put (Environment.update x s env, succ i)
               return (iVariable x)
    where
      getConstant (Token.Boolean b) = Just (Scalar.Boolean b)
      getConstant (Token.Real    r) = Just (Scalar.Real    r)
      getConstant _                 = Nothing

pLambda = special "lambda" $ liftA2 iLambda (parens (many identifier)) expression
pCons = special "cons" $ liftA2 iCons expression expression
pList = special "list" $ liftA  iList (many expression)
pConsStar = special "cons*" $ liftA iConsStar (many expression)
pIf = special "if" $ liftA3 iIf expression expression expression
pLet = special "let" $ liftA2 iLet bindings expression
    where
      bindings = parens (many binding)
      binding  = parens $ liftA2 (,) identifier expression
pLetrec = special "letrec" $ liftA2 iLetrec bindings expression
    where
      bindings = parens (many binding)
      binding  = parens $ liftA3 (,,) identifier (parens (many identifier)) expression
pCond = special "cond" $ liftA iCond (many branch)
    where
      branch   = parens $ liftA2 (,) expression expression

pApplication = liftA2 iApplication expression (many expression)

expression = atom <|> form
    where
      atom = pVariable <|> pConstant
      form = parens $
                 try pLambda
             <|> try pCons
             <|> try pList
             <|> try pConsStar
             <|> try pIf
             <|> try pLet
             <|> try pLetrec
             <|> try pCond
             <|> pApplication

parseAndConvertConstants :: String -> (SurfaceExpr, ScalarEnvironment)
parseAndConvertConstants
    = ((either (\_ -> error "parse error") id) *** fst)
    . flip runState (initialEnvironment, 0)
    . runParserT expression () ""
    . scan
    where
      initialEnvironment
          = Environment.fromList
            [ (nil,   Scalar.Nil          )
            , (true,  Scalar.Boolean True )
            , (false, Scalar.Boolean False)
            ]

class Render f where
    render :: Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t

instance Render Variable where
    render (Variable x) = x

instance Render Lambda where
    render (Lambda xs b) = "(lambda (" ++ unwords xs ++ ") " ++ (pretty b) ++ ")"

instance Render Application where
    render (Application operator operands) = concat[ "("
                                                   , unwords $ map pretty (operator:operands)
                                                   ,  ")"
                                                   ]
instance Render Cons where
    render (Cons x y) = "(cons " ++ (pretty x) ++ " " ++ (pretty y) ++ ")"

instance Render List where
    render (List xs) = "(list " ++ (unwords $ map pretty xs) ++ ")"

instance Render ConsStar where
    render (ConsStar xs) = "(cons* " ++ (unwords $ map pretty xs) ++ ")"

instance Render If where
    render (If p c a) = "(if " ++ unwords [pretty p, pretty c, pretty a] ++ ")"

instance Render Let where
    render (Let bindings body) = "(let (" ++ (unwords $ map renderBinding bindings)
                             ++ " " ++ (pretty body) ++ ")"
        where renderBinding (name, expr) = "(" ++ name ++ " " ++ pretty expr ++ ")"

instance Render Letrec where
    render (Letrec bindings body) = "(letrec (" ++ (unwords $ map renderBinding bindings)
                                    ++ ") " ++ (pretty body) ++ ")"
        where renderBinding (name, params, expr) = "(" ++ name ++ " (" ++ unwords params ++ ") "
                                                   ++ (pretty expr) ++ ")"

instance Render Cond where
    render (Cond branches) = "(cond " ++ (unwords $ map renderBranch branches) ++ ")"
        where renderBranch (c, b) = "(" ++ (pretty c) ++ " " ++ (pretty b) ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
    render (Inl x) = render x
    render (Inr y) = render y

type Stage1
    =   Variable
    :+: Lambda
    :+: Application
    :+: Cons
    :+: List
    :+: ConsStar
    :+: If
    :+: Let
    :+: Letrec

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

elimCond :: Expr Surface -> Expr Stage1
elimCond = foldExpr elimCondAlgebra

class Functor f => ElimCond f where
    elimCondAlgebra :: f (Expr Stage1) -> Expr Stage1

instance ElimCond Variable where
    elimCondAlgebra (Variable x) = iVariable x

instance ElimCond Lambda where
    elimCondAlgebra (Lambda x b) = iLambda x b

instance ElimCond Application where
    elimCondAlgebra (Application operator operands) = iApplication operator operands

instance ElimCond Cons where
    elimCondAlgebra (Cons x y) = iCons x y

instance ElimCond List where
    elimCondAlgebra (List xs) = iList xs

instance ElimCond ConsStar where
    elimCondAlgebra (ConsStar xs) = iConsStar xs

instance ElimCond If where
    elimCondAlgebra (If p c a) = iIf p c a

instance ElimCond Let where
    elimCondAlgebra (Let bindings body) = iLet bindings body

instance ElimCond Letrec where
    elimCondAlgebra (Letrec bindings body) = iLetrec bindings body

instance ElimCond Cond where
    elimCondAlgebra (Cond branches)
        = foldr wrapIf (iVariable "#:nil") branches
          where wrapIf (c, b) e = iIf c b e

instance (ElimCond f, ElimCond g) => ElimCond (f :+: g) where
    elimCondAlgebra (Inl x) = elimCondAlgebra x
    elimCondAlgebra (Inr x) = elimCondAlgebra x

type Stage2
    =   Variable
    :+: Lambda
    :+: Application
    :+: Cons
    :+: List
    :+: ConsStar
    :+: Let
    :+: Letrec

elimIf :: Expr Stage1 -> Expr Stage2
elimIf = foldExpr elimIfAlgebra

class Functor f => ElimIf f where
    elimIfAlgebra :: f (Expr Stage2) -> Expr Stage2

instance ElimIf Variable where
    elimIfAlgebra (Variable x) = iVariable x

instance ElimIf Lambda where
    elimIfAlgebra (Lambda x b) = iLambda x b

instance ElimIf Application where
    elimIfAlgebra (Application operator operands) = iApplication operator operands

instance ElimIf Cons where
    elimIfAlgebra (Cons x y) = iCons x y

instance ElimIf List where
    elimIfAlgebra (List xs) = iList xs

instance ElimIf ConsStar where
    elimIfAlgebra (ConsStar xs) = iConsStar xs

instance ElimIf If where
    elimIfAlgebra (If p c a) = iApplication (iVariable "#:if-procedure") [(thunk c), (thunk a)]
        where thunk e = iLambda [] e

instance ElimIf Let where
    elimIfAlgebra (Let bindings body) = iLet bindings body

instance ElimIf Letrec where
    elimIfAlgebra (Letrec bindings body) = iLetrec bindings body

instance (ElimIf f, ElimIf g) => ElimIf (f :+: g) where
    elimIfAlgebra (Inl x) = elimIfAlgebra x
    elimIfAlgebra (Inr x) = elimIfAlgebra x
