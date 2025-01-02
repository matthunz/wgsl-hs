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
    Attribute (..),
    vertex,
    fragment,
    Shader (..),
    fn,
    exportFn,
    toString,
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
stmtToString stmt = let (_, _, s) = stmtToString' 0 0 stmt in s

stmtToString' :: Int -> Int -> Stmt a -> (a, Int, String)
stmtToString' _ i (PureStmt a) = (a, i, "")
stmtToString' indent i (MapStmt f a) = let (a', i', s) = stmtToString' indent i a in (f a', i', s)
stmtToString' indent i (AppStmt f a) =
  let (f', i1, s1) = stmtToString' indent i f
      (a', i2, s2) = stmtToString' indent i1 a
   in (f' a', i2, s1 ++ s2)
stmtToString' indent i (BindStmt a f) =
  let (a', i1, s1) = stmtToString' indent i a
      (b, i2, s2) = stmtToString' indent i1 (f a')
   in (b, i2, s1 ++ s2)
stmtToString' indent i (ExprStmt a) = (buildWGSL a, i, replicate indent ' ' ++ exprToString (buildWGSL a))
stmtToString' indent i (VarStmt a) =
  ( pure (VarExpr ("v" ++ show i)),
    i + 1,
    replicate indent ' ' ++ "let v" ++ show i ++ " = " ++ exprToString (buildWGSL a) ++ ";\n"
  )

newtype Attribute = Attribute String

vertex :: Attribute
vertex = Attribute "vertex"

fragment :: Attribute
fragment = Attribute "fragment"

attrToString :: Attribute -> String
attrToString (Attribute value) = "@" ++ value

data Shader a where
  PureShader :: a -> Shader a
  MapShader :: (a -> b) -> Shader a -> Shader b
  AppShader :: Shader (a -> b) -> Shader a -> Shader b
  BindShader :: Shader a -> (a -> Shader b) -> Shader b
  FnShader :: [Attribute] -> Stmt (WGSL (Expr a)) -> Shader (Expr a)
  ExportFnShader :: String -> Stmt (WGSL (Expr a)) -> Shader (Expr a)

instance Functor Shader where
  fmap = MapShader

instance Applicative Shader where
  pure = PureShader
  (<*>) = AppShader

instance Monad Shader where
  (>>=) = BindShader

fn :: [Attribute] -> Stmt (WGSL (Expr a)) -> Shader (Expr a)
fn = FnShader

exportFn :: String -> Stmt (WGSL (Expr a)) -> Shader (Expr a)
exportFn = ExportFnShader

toString :: Shader a -> String
toString shader = let (_, _, s) = toString' 0 shader in s

toString' :: Int -> Shader a -> (a, Int, String)
toString' i (PureShader a) = (a, i, "")
toString' i (MapShader f a) = let (a', i', s) = toString' i a in (f a', i', s)
toString' i (AppShader f a) =
  let (f', i1, s1) = toString' i f
      (a', i2, s2) = toString' i1 a
   in (f' a', i2, s1 ++ s2)
toString' i (BindShader a f) =
  let (a', i1, s1) = toString' i a
      (b, i2, s2) = toString' i1 (f a')
   in (b, i2, s1 ++ s2)
toString' i (FnShader attrs stmt) =
  let (a, i', s) = stmtToString' 4 (i + 1) stmt
   in ( buildWGSL a,
        i',
        (concat $ map attrToString attrs)
          ++ "\nfn v"
          ++ show i
          ++ "() {\n"
          ++ s
          ++ "}\n"
      )
toString' i (ExportFnShader name stmt) =
  let (a, i', s) = stmtToString' 4 i stmt
   in (buildWGSL a, i', "fn " ++ name ++ "() {\n" ++ s ++ "}\n")
