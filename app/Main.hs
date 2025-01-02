{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.WGSL

v :: Shader (Expr Int)
v = fn [vertex] $ do
  x <- var (2 + 2)
  y <- var (x * 3)
  return (x + y)

f :: Shader (Expr Int)
f = fn [fragment] $ do
  x <- var (2 + 2)
  y <- var (x * 3)
  return (x + y)

s :: Shader ()
s = do
  _ <- v
  _ <- f
  return ()

main :: IO ()
main = putStrLn (toString s)
