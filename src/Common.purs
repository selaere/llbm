module Main.Common where

import Prelude

import Data.Monoid (guard)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (under)
import Data.Ord (lessThanOrEq, greaterThanOrEq)
import Data.Tuple (Tuple(..))

infixr 9 compose as ∘

mapCompose ∷ ∀f t a b. Functor f ⇒ (a → b) → (t → f a) → t → f b
mapCompose = (∘) ∘ map
infixr 9 mapCompose as <∘>

doWhen ∷ ∀a. Boolean → (a → a) → a → a
doWhen = under Endo ∘ guard

infixr 4 eq as ≡
infixr 4 notEq as ≢

infixr 5 append as ⋄

infixr 6 Tuple as ⍪
infixr 6 type Tuple as ⍪

infixl 4 lessThanOrEq as ≤
infixl 4 greaterThanOrEq as ≥

mapMap ∷ ∀a b f g. Functor f ⇒ Functor g ⇒ (a → b) → f (g a) → f (g b)
mapMap = map ∘ map
infixl 4 mapMap as <<$>>

flippedMapMap ∷ ∀a b f g. Functor f ⇒ Functor g ⇒ f (g a) → (a → b) → f (g b)
flippedMapMap = flip mapMap
infixl 1 flippedMapMap as <<#>>

bool ∷ ∀a. a → a → Boolean → a
bool a b c = if c then b else a