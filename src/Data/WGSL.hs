{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.WGSL
  ( Lit (..),
    Expr (..),
    exprToString,
    WGSL (..),
    intWGSL,
    toString,
    buildWGSL,
  )
where

data Lit a where
  IntLit :: Int -> Lit Int
  FloatLit :: Float -> Lit Float
  BoolLit :: Bool -> Lit Bool

data Expr a where
  LitExpr :: Lit a -> Expr a
  FnCall :: String -> [Expr a] -> Expr a
  NegExpr :: (Num a) => Expr a -> Expr a
  AddExpr :: (Num a) => Expr a -> Expr a -> Expr a
  MulExpr :: (Num a) => Expr a -> Expr a -> Expr a

exprToString :: Expr a -> String
exprToString (LitExpr (IntLit a)) = show a
exprToString (LitExpr (FloatLit a)) = show a
exprToString (LitExpr (BoolLit a)) = show a
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

toString :: WGSL (Expr a) -> String
toString = exprToString . buildWGSL

buildWGSL :: WGSL a -> a
buildWGSL (PureWGSL a) = a
buildWGSL (MapWGSL f a) = f (buildWGSL a)
buildWGSL (AppWGSL f a) = (buildWGSL f) (buildWGSL a)
buildWGSL (BindWGSL a f) = buildWGSL (f (buildWGSL a))
buildWGSL (ExprWGSL a) = a
