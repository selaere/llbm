module Main.JsStuff where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Newtype (unwrap)
import Main.Common (doWhen, (∘))

foreign import v3 ∷ Fn2 String Int Int

murmur3 ∷ String → Int → Int
murmur3 = runFn2 v3

foreign import formatSecs ∷ Number → String
foreign import showSecs   ∷ Number → String

formatTime ∷ Instant → String
formatTime = formatSecs ∘ unwrap ∘ unInstant

showTime ∷ Instant → String
showTime = (\x→ doWhen (x < 1602763036000.0) (append "≈") (showSecs x)) ∘ unwrap ∘ unInstant