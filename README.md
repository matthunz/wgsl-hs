# WGSL-hs

```hs
module Main where

import Data.WGSL

vert :: Shader (Expr Int)
vert =
  fn
    [vertex]
    (arg [location 0])
    ( \i -> do
        x <- var (i + 2)
        y <- var (x * 3)
        return (x + y)
    )

frag :: Shader (Expr Int)
frag =
  fn
    [fragment]
    (pure ())
    ( \_ -> do
        x <- var (2 + 2)
        y <- var (x * 3)
        return (x + y)
    )

shader :: Shader ()
shader = do
  _ <- vert
  _ <- frag
  return ()

main :: IO ()
main = putStrLn (toString shader)
```
