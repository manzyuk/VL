module VL.Rename where

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

freshName :: Name -> Supply Name
freshName prefix = do i <- get
                      let name = prefix ++ show i
                      put (succ i)
                      return name

tempName :: Supply Name
tempName = freshName "#:temp-"

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
    = do x <- tempName
         b <- body
         return (x, rename (Map.singleton arg x) b)

instance Uniquify ApplicationOneArg where
    uniquifyAlg (ApplicationOneArg operator operand)
        = liftM2 mkApplicationOneArg operator operand

instance Uniquify Cons where
    uniquifyAlg (Cons e1 e2) = liftM2 mkCons e1 e2

instance Uniquify LetrecOneArg where
    uniquifyAlg (LetrecOneArg bindings body)
        = do vs' <- sequence [ tempName           | v      <- vs ]
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