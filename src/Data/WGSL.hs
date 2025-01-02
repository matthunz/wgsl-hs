{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.WGSL
  ( Lit (..),
    Expr (..),
    exprToString,
    WGSL (..),
    intWGSL,
    wgslExprToString,
    buildWGSL,
    Stmt (..),
    var,
    stmtToString,
  )
where

data Lit a where
  IntLit :: Int -> Lit Int
  FloatLit :: Float -> Lit Float
  BoolLit :: Bool -> Lit Bool

data Expr a where
  LitExpr :: Lit a -> Expr a
  VarExpr :: String -> Expr a
  FnCall :: String -> [Expr a] -> Expr a
  NegExpr :: (Num a) => Expr a -> Expr a
  AddExpr :: (Num a) => Expr a -> Expr a -> Expr a
  MulExpr :: (Num a) => Expr a -> Expr a -> Expr a

exprToString :: Expr a -> String
exprToString (LitExpr (IntLit a)) = show a
exprToString (LitExpr (FloatLit a)) = show a
exprToString (LitExpr (BoolLit a)) = show a
exprToString (VarExpr a) = a
exprToString (FnCall name args) = name ++ "(" ++ unwords (map exprToString args) ++ ")"
exprToString (NegExpr a) = "-" ++ exprToString a
exprToString (AddExpr a b) = exprToString a ++ " + " ++ exprToString b
exprToString (MulExpr a b) = exprToString a ++ " * " ++ exprToString b

data WGSL a where
  PureWGSL :: a -> WGSL a
  MapWGSL :: (a -> b) -> WGSL a -> WGSL b
  AppWGSL :: WGSL (a -> b) -> WGSL a -> WGSL b
  BindWGSL :: WGSL a -> (a -> WGSL b) -> WGSL b
  ExprWGSL :: Expr a -> WGSL (Expr a)

instance Functor WGSL where
  fmap = MapWGSL

instance Applicative WGSL where
  pure = PureWGSL
  (<*>) = AppWGSL

instance Monad WGSL where
  (>>=) = BindWGSL

intWGSL :: Int -> WGSL (Expr Int)
intWGSL a = return (LitExpr (IntLit a))

instance Num (WGSL (Expr Int)) where
  (+) a b = do
    a' <- a
    b' <- b
    return (AddExpr a' b')

  (*) a b = do
    a' <- a
    b' <- b
    return (MulExpr a' b')
  abs a = do
    a' <- a
    return $ FnCall "abs" [a']
  signum a = do
    a' <- a
    return $ FnCall "sign" [a']
  fromInteger i = pure (LitExpr (IntLit (fromInteger i)))
  negate a = do
    a' <- a
    return $ NegExpr a'

wgslExprToString :: WGSL (Expr a) -> String
wgslExprToString = exprToString . buildWGSL

buildWGSL :: WGSL a -> a
buildWGSL (PureWGSL a) = a
buildWGSL (MapWGSL f a) = f (buildWGSL a)
buildWGSL (AppWGSL f a) = (buildWGSL f) (buildWGSL a)
buildWGSL (BindWGSL a f) = buildWGSL (f (buildWGSL a))
buildWGSL (ExprWGSL a) = a

data Stmt a where
  PureStmt :: a -> Stmt a
  MapStmt :: (a -> b) -> Stmt a -> Stmt b
  AppStmt :: Stmt (a -> b) -> Stmt a -> Stmt b
  BindStmt :: Stmt a -> (a -> Stmt b) -> Stmt b
  ExprStmt :: WGSL (Expr a) -> Stmt (Expr a)
  VarStmt :: WGSL (Expr a) -> Stmt (WGSL (Expr a))

instance Functor Stmt where
  fmap = MapStmt

instance Applicative Stmt where
  pure = PureStmt
  (<*>) = AppStmt

instance Monad Stmt where
  (>>=) = BindStmt

var :: WGSL (Expr a) -> Stmt (WGSL (Expr a))
var = VarStmt

stmtToString :: Stmt a -> String
stmtToString stmt = let (_, _, s) = stmtToString' 0 stmt in s

stmtToString' :: Int -> Stmt a -> (a, Int, String)
stmtToString' i (PureStmt a) = (a, i, "")
stmtToString' i (MapStmt f a) = let (a', i', s) = stmtToString' i a in (f a', i', s)
stmtToString' i (AppStmt f a) =
  let (f', i1, s1) = stmtToString' i f
      (a', i2, s2) = stmtToString' i1 a
   in (f' a', i2, s1 ++ s2)
stmtToString' i (BindStmt a f) =
  let (a', i1, s1) = stmtToString' i a
      (b, i2, s2) = stmtToString' i1 (f a')
   in (b, i2, s1 ++ s2)
stmtToString' i (ExprStmt a) = (buildWGSL a, i, exprToString (buildWGSL a))
stmtToString' i (VarStmt a) =
  ( pure (VarExpr ("v" ++ show i)),
    i + 1,
    "let v" ++ show i ++ " = " ++ exprToString (buildWGSL a) ++ ";\n"
  )
