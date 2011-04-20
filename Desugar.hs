{-# LANGUAGE TypeOperators, FlexibleContexts, TemplateHaskell #-}

module VL.Desugar (desugar, prepare) where

import VL.Common
import VL.Coproduct
import VL.Expression
import VL.FixedPoint

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

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

elimConditionals :: Expr Surface -> Expr Stage1
elimConditionals = foldExpr elimConditionalsAlg

class Functor f => ElimConditionals f where
    elimConditionalsAlg :: f(Expr Stage1) -> Expr Stage1

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

elimIf :: Expr Stage1 -> Expr Stage2
elimIf = foldExpr elimIfAlg

class Functor f => ElimIf f where
    elimIfAlg :: f (Expr Stage2) -> Expr Stage2

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

elimLet :: Expr Stage2 -> Expr Stage3
elimLet = foldExpr elimLetAlg

class Functor f => ElimLet f where
    elimLetAlg :: f (Expr Stage3) -> Expr Stage3

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

elimManyArgs :: Expr Stage3 -> Expr Stage4
elimManyArgs = foldExpr elimManyArgsAlg

class Functor f => ElimManyArgs f where
    elimManyArgsAlg :: f (Expr Stage4) -> Expr Stage4

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
nestLambdas :: [Name] -> Expr Stage4 -> (Name, Expr Stage4)
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
            => Int -> Expr f -> Expr f
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
elimList :: Expr Stage4 -> Expr Core
elimList = foldExpr elimListAlg

class Functor f => ElimList f where
    elimListAlg :: f (Expr Core) -> Expr Core

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
foldr' step x0 []     = x0
foldr' step x0 [x1]   = x1
foldr' step x0 (x:xs) = step x (foldr' step x0 xs)

instance (ElimList f, ElimList g) => ElimList (f :+: g) where
    elimListAlg (Inl x) = elimListAlg x
    elimListAlg (Inr x) = elimListAlg x

desugar :: SurfaceExpression -> CoreExpression
desugar = elimList
        . elimManyArgs
        . elimLet
        . elimIf
        . elimConditionals

-- Variable renaming
type Dictionary = Map Name Name

rename :: Dictionary -> CoreExpression -> CoreExpression
rename dict = foldExpr (renameAlg dict)

class Functor f => Rename f where
    renameAlg :: Dictionary -> f CoreExpression -> CoreExpression

maybeRename :: Dictionary -> Name -> Name
maybeRename dict name = fromMaybe name (Map.lookup name dict)

instance Rename Variable where
    renameAlg dict (Variable x) = mkVariable x'
        where
          x' = maybeRename dict x

instance Rename LambdaOneArg where
    renameAlg dict (LambdaOneArg arg body) = mkLambdaOneArg arg' body
        where
          arg' = maybeRename dict arg

instance Rename ApplicationOneArg where
    renameAlg dict (ApplicationOneArg operator operand)
        = mkApplicationOneArg operator operand

instance Rename Cons where
    renameAlg dict (Cons e1 e2) = mkCons e1 e2

instance Rename LetrecOneArg where
    renameAlg dict (LetrecOneArg bindings body)
        = mkLetrecOneArg bindings' body
        where
          bindings' = [ ( maybeRename dict v
                        , maybeRename dict u
                        , e
                        )
                      | (v, u, e) <- bindings
                      ]

instance (Rename f, Rename g) => Rename (f :+: g) where
    renameAlg dict (Inl x) = renameAlg dict x
    renameAlg dict (Inr x) = renameAlg dict x

-- Uniquification: making bound variables unique
type Supply = State Int

freshName :: Supply Name
freshName = do i <- get
               let name = "#:temp-" ++ show i
               put (succ i)
               return name

uniquify :: CoreExpression -> CoreExpression
uniquify = flip evalState 0 . foldExpr uniquifyAlg

class Functor f => Uniquify f where
    uniquifyAlg :: f (Supply CoreExpression) -> Supply CoreExpression

instance Uniquify Variable where
    uniquifyAlg (Variable x) = return (mkVariable x)

instance Uniquify LambdaOneArg where
    uniquifyAlg (LambdaOneArg arg body)
        = do (arg', body') <- uniquifyLambda arg body
             return $ mkLambdaOneArg arg' body'

uniquifyLambda :: Name -> Supply CoreExpression -> Supply (Name, CoreExpression)
uniquifyLambda arg body
    = do x <- freshName
         b <- body
         return (x, rename (Map.singleton arg x) b)

instance Uniquify ApplicationOneArg where
    uniquifyAlg (ApplicationOneArg operator operand)
        = liftM2 mkApplicationOneArg operator operand

instance Uniquify Cons where
    uniquifyAlg (Cons e1 e2) = liftM2 mkCons e1 e2

instance Uniquify LetrecOneArg where
    uniquifyAlg (LetrecOneArg bindings body)
        = do vs' <- sequence [ freshName          | v      <- vs ]
             ls' <- sequence [ uniquifyLambda u e | (u, e) <- ls ]
             b   <- body
             let dict = Map.fromList $ zip vs vs'
                 bindings' = [ (v', u', rename dict e')
                             | (v', (u', e')) <- zip vs' ls'
                             ]
                 b' = rename dict b
             return $ mkLetrecOneArg bindings' b'
        where
          vs = [ v      | (v, _, _) <- bindings ]
          ls = [ (u, e) | (_, u, e) <- bindings ]

instance (Uniquify f, Uniquify g) => Uniquify (f :+: g) where
    uniquifyAlg (Inl x) = uniquifyAlg x
    uniquifyAlg (Inr x) = uniquifyAlg x

-- Uniquification is necessary for the correctness of `pushLetrec'.
prepare :: SurfaceExpression -> CoreExpression
prepare = uniquify . desugar