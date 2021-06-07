{-# LANGUAGE TypeOperators, FlexibleContexts, TemplateHaskell #-}
module VL.Language.Desugar (desugar) where

import VL.Language.Common
import VL.Language.Syntax

import VL.Alacarte.Coproduct
import VL.Alacarte.FixedPoint

-- Elimination of derived conditionals (OR, AND, COND)
type Stage1  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: If
            :+: Let
            :+: LetrecManyArgs

elimConditionals :: Syntax Surface -> Syntax Stage1
elimConditionals = foldSyntax elimConditionalsAlg

class Functor f => ElimConditionals f where
    elimConditionalsAlg :: f(Syntax Stage1) -> Syntax Stage1

$(deriveAlgebraInstances ''ElimConditionals
  [ ''Variable
  , ''LambdaManyArgs
  , ''ApplicationManyArgs
  , ''Cons
  , ''List
  , ''ConsStar
  , ''If
  , ''Let
  , ''LetrecManyArgs
  ])

instance ElimConditionals Or where
    elimConditionalsAlg (Or xs) = foldr wrap (mkVariable false) xs
        where
          wrap x y = mkIf x (mkVariable true) y

instance ElimConditionals And where
    elimConditionalsAlg (And xs) = foldr wrap (mkVariable true) xs
        where
          wrap x y = mkIf x y (mkVariable false)

instance ElimConditionals Not where
    elimConditionalsAlg (Not x) = mkIf x (mkVariable false) (mkVariable true)

instance ElimConditionals Cond where
    elimConditionalsAlg (Cond clauses) = foldr wrap (mkVariable nil) clauses
        where
          wrap (t, e) e' = mkIf t e e'

instance (ElimConditionals f, ElimConditionals g) =>
    ElimConditionals (f :+: g) where
        elimConditionalsAlg (Inl x) = elimConditionalsAlg x
        elimConditionalsAlg (Inr x) = elimConditionalsAlg x

-- Elimination of IF
type Stage2  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: Let
            :+: LetrecManyArgs

elimIf :: Syntax Stage1 -> Syntax Stage2
elimIf = foldSyntax elimIfAlg

class Functor f => ElimIf f where
    elimIfAlg :: f (Syntax Stage2) -> Syntax Stage2

$(deriveAlgebraInstances ''ElimIf
  [ ''Variable
  , ''LambdaManyArgs
  , ''ApplicationManyArgs
  , ''Cons
  , ''List
  , ''ConsStar
  , ''Let
  , ''LetrecManyArgs
  ])

instance ElimIf If where
    elimIfAlg (If predicate consequent alternate)
        = mkApplicationManyArgs (mkVariable "#:if-procedure")
                   [predicate, thunk consequent, thunk alternate]
        where
          thunk e = mkLambdaManyArgs [] e

instance (ElimIf f, ElimIf g) => ElimIf (f :+: g) where
    elimIfAlg (Inl x) = elimIfAlg x
    elimIfAlg (Inr x) = elimIfAlg x

-- Elimination of LET
type Stage3  =  Variable
            :+: LambdaManyArgs
            :+: ApplicationManyArgs
            :+: Cons
            :+: List
            :+: ConsStar
            :+: LetrecManyArgs

elimLet :: Syntax Stage2 -> Syntax Stage3
elimLet = foldSyntax elimLetAlg

class Functor f => ElimLet f where
    elimLetAlg :: f (Syntax Stage3) -> Syntax Stage3

$(deriveAlgebraInstances ''ElimLet
  [ ''Variable
  , ''LambdaManyArgs
  , ''ApplicationManyArgs
  , ''Cons
  , ''List
  , ''ConsStar
  , ''LetrecManyArgs
  ])

instance ElimLet Let where
    elimLetAlg (Let bindings body)
        = mkApplicationManyArgs (mkLambdaManyArgs args body) vals
        where
          (args, vals) = unzip bindings

instance (ElimLet f, ElimLet g) => ElimLet (f :+: g) where
    elimLetAlg (Inl x) = elimLetAlg x
    elimLetAlg (Inr x) = elimLetAlg x

-- Elimination of many arguments (in lambdas, applications, and LETREC bindings)
type Stage4  =  Variable
            :+: LambdaOneArg
            :+: ApplicationOneArg
            :+: Cons
            :+: List
            :+: ConsStar
            :+: LetrecOneArg

elimManyArgs :: Syntax Stage3 -> Syntax Stage4
elimManyArgs = foldSyntax elimManyArgsAlg

class Functor f => ElimManyArgs f where
    elimManyArgsAlg :: f (Syntax Stage4) -> Syntax Stage4

$(deriveAlgebraInstances ''ElimManyArgs
  [ ''Variable
  , ''Cons
  , ''List
  , ''ConsStar
  ])

instance ElimManyArgs LambdaManyArgs where
    elimManyArgsAlg (LambdaManyArgs args body)
        = mkLambdaOneArg arg body'
        where
          (arg, body') = nestLambdas args body

-- Transform a lambda that takes possibly many arguments into a
-- combination of nested lambdas that take only one argument,
-- introducing suitable argument destructuring.  In particular:
--
-- (lambda () e)
-- ~> (lambda (#:ignored) e)
--
-- and
--
-- (lambda (x1 x2) e)
-- ~> (lambda (#:args)
--      ((lambda (x1)
--         ((lambda (x2)
--            e)
--          (cdr #:args)))
--       (car #:args)))
nestLambdas :: [Name] -> Syntax Stage4 -> (Name, Syntax Stage4)
nestLambdas []    body = ("#:ignored", body)
nestLambdas [arg] body = (arg, body)
nestLambdas args  body = ("#:args", body'')
    where
      p             = mkVariable "#:args"
      n             = length args
      argn          = last args
      body'         = with argn (cdnr (n-1) p) body
      body''        = foldr wrap body' (zip args [0..n-2])
      wrap (x, k) e = with x (cadnr k p) e
      with x v e    = mkApplicationOneArg (mkLambdaOneArg x e) v

cdnr, cadnr :: (ApplicationOneArg :<: f, Variable :<: f)
            => Int -> Syntax f -> Syntax f
cdnr n = compose (replicate n cdr)
    where
      compose = foldr (.) id
      cdr = mkApplicationOneArg (mkVariable "cdr")

cadnr n = car . cdnr n
    where
      car = mkApplicationOneArg (mkVariable "car")

instance ElimManyArgs ApplicationManyArgs where
    elimManyArgsAlg (ApplicationManyArgs operator operands)
        = mkApplicationOneArg operator (mkConsStar operands)

instance ElimManyArgs LetrecManyArgs where
    elimManyArgsAlg (LetrecManyArgs bindings body)
        = mkLetrecOneArg bindings' body
        where
          bindings' = [ (name, arg, e')
                      | (name, args, e) <- bindings
                      , let (arg, e') = nestLambdas args e
                      ]

instance (ElimManyArgs f, ElimManyArgs g) => ElimManyArgs (f :+: g) where
    elimManyArgsAlg (Inl x) = elimManyArgsAlg x
    elimManyArgsAlg (Inr x) = elimManyArgsAlg x

-- Elimination of LIST and CONS*
elimList :: Syntax Stage4 -> Syntax Core
elimList = foldSyntax elimListAlg

class Functor f => ElimList f where
    elimListAlg :: f (Syntax Core) -> Syntax Core

$(deriveAlgebraInstances ''ElimList
  [ ''Variable
  , ''LambdaOneArg
  , ''ApplicationOneArg
  , ''Cons
  , ''LetrecOneArg
  ])

instance ElimList List where
    elimListAlg (List xs) = foldr mkCons (mkVariable nil) xs

instance ElimList ConsStar where
    elimListAlg (ConsStar xs) = foldr' mkCons (mkVariable nil) xs

foldr' :: (b -> b -> b) -> b -> [b] -> b
foldr' _    x0 []     = x0
foldr' _    _  [x1]   = x1
foldr' step x0 (x:xs) = step x (foldr' step x0 xs)

instance (ElimList f, ElimList g) => ElimList (f :+: g) where
    elimListAlg (Inl x) = elimListAlg x
    elimListAlg (Inr x) = elimListAlg x

desugar :: SurfaceSyntax -> CoreSyntax
desugar = elimList
        . elimManyArgs
        . elimLet
        . elimIf
        . elimConditionals
