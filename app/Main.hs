{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.WGSL

f :: Shader (Expr Int)
f = fn $ do
  x <- var (2 + 2)
  y <- var (x * 3)
  return (x + y)

s :: Shader ()
s = do
  _ <- f
  return ()

main :: IO ()
main = putStrLn (toString s)
