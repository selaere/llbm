module Main.Mode (Mode(..), allB, fromString, merge, intersect, subtract, fun, ε, (∩)) where

import Prelude

import Control.Alternative (guard)
import Data.Array (catMaybes, elemIndex, foldl, length, mapWithIndex, (..))
import Data.Hashable (class Hashable, hash)
import Data.Int.Bits (complement, shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, fromCodePointArray, toCodePointArray, toUpper)
import Main.Common (doWhen, (<∘>), (∘), (≢), (⍪))
import Unsafe.Coerce (unsafeCoerce)


data Mode = Mode Int Int

ε ∷ Mode
ε = Mode 0 0

instance Show Mode where show = toString
derive instance Eq Mode
derive instance Ord Mode
instance Bounded Mode where
  bottom = ε
  top = fromString ∘ toUpper $ fromCodePointArray modeChars
instance Semigroup Mode where append = merge
instance Monoid Mode where mempty = ε
instance Hashable Mode where hash (Mode m n) = hash (m⍪n)

modeChars ∷ Array CodePoint
modeChars = toCodePointArray "bwuipscmhaedtnvklgjyrf"

allB ∷ Array Mode
allB = Mode 0 <$> shl 1 <$> 0 .. (length modeChars - 1)

toUpperDumb ∷ CodePoint → CodePoint
toUpperDumb = unsafeCoerce (_-32)
toLowerDumb ∷ CodePoint → CodePoint
toLowerDumb = unsafeCoerce (_+32)

toString ∷ Mode → String
toString (Mode _  0) = "ε"
toString (Mode xf x) = fromCodePointArray ∘ catMaybes $ mapWithIndex go modeChars
  where
    go i code = guard (x.&.m ≢ 0) $> doWhen (xf.&.m ≢ 0) toUpperDumb code
      where m = shl 1 i

fromString ∷ String → Mode
fromString = foldl merge ε ∘ go <∘> toCodePointArray
  where 
  go char = 
    case elemIndex char modeChars of
      Just x  → Mode 0 (shl 1 x)
      Nothing → case elemIndex (toLowerDumb char) modeChars of
        Just x  → join Mode (shl 1 x)
        Nothing → ε

merge ∷ Mode → Mode → Mode
merge (Mode xf x) (Mode yf y) = Mode (xf .|. yf) (x .|. y)

intersect ∷ Mode → Mode → Mode
intersect (Mode xf x) (Mode yf y) = Mode (xf .&. yf .&. x .&. y) (x .&. y)
infixr 6 intersect as ∩

subtract ∷ Mode → Mode → Mode
subtract (Mode xf x) (Mode _ y) = Mode (xf .&. complement y) (x .&. complement y)

fun ∷ Mode → Mode
fun (Mode _ x) = Mode x x
