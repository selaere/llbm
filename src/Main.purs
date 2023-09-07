module Main where

import Prelude hiding (apply)

import Affjax as AJ
import Affjax.ResponseFormat (string)
import Affjax.Web as AJW
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.Error.Class (liftEither)
import DOM.HTML.Indexed as DOM
import Data.Array (catMaybes, unsnoc, elemIndex, length, mapWithIndex, tail, take, zipWith, (..))
import Data.Array.NonEmpty (toArray)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, instant, toDateTime)
import Data.Either (either, hush)
import Data.Foldable (foldl, sum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int as Int
import Data.Int.Bits (shl, (.&.), (.|.))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as M
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (under)
import Data.Number as Number
import Data.String (CodePoint, Pattern(..), fromCodePointArray, toCodePointArray)
import Data.String.Common (split)
import Data.String.Regex (regex, match)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Murmur3 (hash)


infixr 9 compose as ∘

mapCompose ∷ ∀f t a b. Functor f ⇒ (a → b) → (t → f a) → t → f b
mapCompose = (∘) ∘ map
infixr 9 mapCompose as <∘>

newtype Mode = Mode Int
instance Show Mode where show = modeToString
derive instance Eq Mode
derive instance Ord Mode
instance Bounded Mode where
  bottom = Mode 0
  top = modeFromString $ fromCodePointArray modeChars
instance Semigroup Mode where append = modeMerge
instance Monoid Mode where mempty = Mode 0

modeChars ∷ Array CodePoint
modeChars = toCodePointArray "bwuipscmhaedtnvklgjyrf"

allModes ∷ Array Mode
allModes = Mode <$> shl 1 <$> 0 .. (length modeChars - 1)

modeToString ∷ Mode → String
modeToString (Mode 0   ) = "ε"
modeToString (Mode mode) = modeChars
  # mapWithIndex (\i x→ x <$ guard (mode .&. (1 `shl` i) /= 0))
  # catMaybes # fromCodePointArray

modeFromString ∷ String → Mode
modeFromString =
  Mode ∘ foldl (.|.) 0 ∘ (shl 1 ∘ sum ∘ elemIndex `flip` modeChars) <∘> toCodePointArray

modeMerge ∷ Mode → Mode → Mode
modeMerge (Mode x) (Mode y) = Mode $ x .|. y

type Score =
  { score ∷ Int
  , mode  ∷ Mode
  , date  ∷ Instant
  , owner ∷ String }

score ∷ Int → Mode → Instant → String → Score
score = {score: _, mode: _, date: _, owner: _}

--derive instance Eq Generic _

parseLine ∷ String → Maybe Score
parseLine a = join (match <$> reg <@> a) <#> toArray >>= tail >>= case _ of
  [s, m, d, o] -> score <$> (s >>= Int.fromString) 
                        <*> (m <#> modeFromString)
                        <*> (d >>= toInstant)
                        <*> o
  _ -> Nothing
  where 
    reg = hush $ regex "^([^ ]*) ([^ ]*) ([^ ]*) (.*)$" mempty
    toInstant = Number.fromString >=> instant ∘ Milliseconds ∘ (*) 1000.0

bee ∷ ∀f a b. Applicative f => (a -> b) -> a -> f b
bee = (pure ∘ _)

parseFile ∷ String → Map Mode Score
parseFile = 
  Map.fromFoldable ∘ lift2 Tuple _.mode identity <∘>
  catMaybes ∘ parseLine <∘> split (Pattern "\n")

main ∷ Effect Unit
main = HA.runHalogenAff do
  datas ← AJW.get string "output.txt"
        >>= liftEither ∘ lmap (error ∘ AJ.printError)
  body ← HA.awaitBody
  runUI component datas.body body

applyWhen ∷ ∀a. Boolean → (a → a) → a → a
applyWhen = under Endo ∘ M.guard

table ∷ State → Array (Array Mode)
table { modes, scol, showEmpty } =
  1 .. (length modes)
  <#> take `flip` modes
  <#> applyWhen scol rotate
  # zipWith (map ∘ append) modes
  # applyWhen showEmpty (append [[mempty]])

color ∷ String → String
color "☭🐝" = "rgb(198,234,169)" -- temporary (elm Murmur3 and ursi/purescript-murmur3 treat unicode differently)
color name = "hsl("<> show hue <>",60%,"<> show lgt <>"%"
  where h = hash 3054 name
        hue = h `mod` 360
        lgt = ((h `div` 360) `mod` 45) + 40

makeCell' ∷ ∀w i. Mode → HH.Node DOM.HTMLtd w i
makeCell' mode a = HH.td a ∘ pure ∘ HH.a [HP.href $ "https://ubq323.website/ffbm#" <> show mode]

makeCell ∷ ∀w i. Mode → Maybe Score → HH.HTML w i
makeCell mode Nothing = makeCell' mode [] [HH.text $ show mode]
makeCell mode (Just {score, owner, date}) =
  makeCell' mode
    [ HP.style $ "background-color:" <> color owner 
    , HP.title $ owner<>" "<> show score <>" in "<> show mode <>" at "<> 
      (either identity identity $ formatDateTime "YYYY-MM-DD" $ toDateTime date)]
    [ HH.text $ show score
    , HH.small_ [HH.text $ " " <> show mode]
    , HH.br_
    , HH.small_ [HH.text owner]
    ]

displayScore ∷ ∀w i. Map Mode Score → Mode → HH.HTML w i
displayScore scores mode = makeCell mode (Map.lookup mode scores)

data Action = Increment | ToggleScol | ToggleShowEmpty

type State =
  { scores ∷ Map Mode Score
  , modes  ∷ Array Mode
  , scol   ∷ Boolean
  , showEmpty ∷ Boolean
  }

rotate ∷ ∀a. Array a → Array a
rotate arr = case unsnoc arr of
  Just { init, last } → [last] <> init
  Nothing -> []

initialState ∷ String → State
initialState a = 
  { scores: parseFile a
  , modes: allModes
  , scol: false
  , showEmpty: true
  }

handleAction ∷ ∀o m. MonadEffect m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment → (H.get >>= log ∘ show) $> unit
    --H.get >>= H.liftEffect (pure log) ∘ show >>= \_→ pure unit --H.modify_ (_ + 2)
  ToggleScol      → H.modify_ (\x→ x {scol      = not x.scol     })
  ToggleShowEmpty → H.modify_ (\x→ x {showEmpty = not x.showEmpty})

render ∷ ∀m. State → H.ComponentHTML Action () m
render state = 
  HH.div_
    [ HH.table_ $ (HH.tr_ ∘ map (displayScore state.scores)) <$> (table state)
    , HH.button [HE.onClick \_→Increment] [HH.text "bee"]
    , HH.input [HP.type_ HP.InputCheckbox, HE.onClick \_→ToggleScol]
    , HH.input [HP.type_ HP.InputCheckbox, HE.onClick \_→ToggleShowEmpty]
    ]

component ∷ ∀query o m. MonadEffect m ⇒ H.Component query String o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
