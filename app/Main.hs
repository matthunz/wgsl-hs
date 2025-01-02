{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.WGSL

s :: Stmt (WGSL (Expr Int))
s = do
  x <- var (2 + 2)
  y <- var (x * 3)
  return (x + y)

main :: IO ()
main = putStrLn (stmtToString s)
