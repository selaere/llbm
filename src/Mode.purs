module Main.Mode (Mode(..), all, fromString, merge, intersect, subtract, ε, (∩)) where

import Prelude

import Control.Alternative (guard)
import Data.Array (catMaybes, elemIndex, foldl, length, mapWithIndex, (..))
import Data.Foldable (sum)
import Data.Hashable (class Hashable)
import Data.Int.Bits (shl, (.&.), (.^.), (.|.))
import Data.Newtype (class Newtype, unwrap)
import Data.String (CodePoint, fromCodePointArray, toCodePointArray)
import Main.Common ((<∘>), (∘), (≢))


newtype Mode = Mode Int

ε ∷ Mode
ε = Mode 0

instance Show Mode where show = toString
derive instance Newtype Mode _
derive instance Eq Mode
derive instance Ord Mode
instance Bounded Mode where
  bottom = ε
  top = fromString $ fromCodePointArray modeChars
instance Semigroup Mode where append = merge
instance Monoid Mode where mempty = ε
instance Hashable Mode where hash (Mode n) = n

modeChars ∷ Array CodePoint
modeChars = toCodePointArray "bwuipscmhaedtnvklgjyrf"

all ∷ Array Mode
all = Mode <$> shl 1 <$> 0 .. (length modeChars - 1)

toString ∷ Mode → String
toString (Mode 0   ) = "ε"
toString (Mode mode) = modeChars
  # mapWithIndex (\i x→ x <$ guard (mode .&. (shl 1 i) ≢ 0))
  # catMaybes # fromCodePointArray

fromString ∷ String → Mode
fromString =
  Mode ∘ foldl (.|.) 0 ∘ (shl 1 ∘ sum ∘ elemIndex `flip` modeChars) <∘> toCodePointArray

merge ∷ Mode → Mode → Mode
merge (Mode x) (Mode y) = Mode $ x .|. y

intersect ∷ Mode → Mode → Mode
intersect (Mode x) (Mode y) = Mode $ x .&. y
infixr 6 intersect as ∩

subtract ∷ Mode → Mode → Mode
subtract (Mode x) (Mode y) = Mode $ x .&. (y .^. unwrap (top∷Mode))
