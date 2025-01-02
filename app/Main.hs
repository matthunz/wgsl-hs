module Main where

import Data.WGSL

s :: WGSL (Expr Int)
s = 1 + 1 * 2

main :: IO ()
main = putStrLn (toString s)
