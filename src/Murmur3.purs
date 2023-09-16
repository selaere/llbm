module Main.Murmur3 where

import Data.Function.Uncurried (Fn2, runFn2)

foreign import v3 ∷ Fn2 String Int Int

hash ∷ String → Int → Int
hash = runFn2 v3