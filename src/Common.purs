module Main.Common where

import Prelude

import Data.Monoid (guard)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (under)

infixr 9 compose as ∘

mapCompose ∷ ∀f t a b. Functor f ⇒ (a → b) → (t → f a) → t → f b
mapCompose = (∘) ∘ map
infixr 9 mapCompose as <∘>

doWhen ∷ ∀a. Boolean → (a → a) → a → a
doWhen = under Endo ∘ guard

infixr 4 eq as ≡
infixr 4 notEq as ≢

infixr 5 append as ⋄