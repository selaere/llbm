module Main where

import Prelude hiding (apply)

import Affjax as AJ
import Affjax.ResponseFormat (string)
import Affjax.Web as AJW
import Control.Alternative (guard)
import Control.Apply (lift2)
import Control.Monad.Error.Class (liftEither)
import DOM.HTML.Indexed as DOM
import Data.Array (catMaybes, cons, elemIndex, length, mapMaybe, mapWithIndex, snoc, tail, take, uncons, unsnoc, zipWith, (!!), (..))
import Data.Array.NonEmpty (toArray)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, fromDateTime, instant, toDateTime, unInstant)
import Data.Either (fromRight, hush, note)
import Data.Foldable (foldl, sum)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Function (on)
import Data.Int as Int
import Data.Int.Bits (shl, (.&.), (.|.))
import Data.Lens (Prism, _2, prism, (%~))
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid as M
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (under, unwrap)
import Data.Number as Number
import Data.String (CodePoint, Pattern(..), fromCodePointArray, joinWith, toCodePointArray)
import Data.String as S
import Data.String.Common (split)
import Data.String.Regex (regex, match)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
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
import Partial.Unsafe (unsafePartial)


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
  # mapWithIndex (\i x→ x <$ guard (mode .&. (shl 1 i) /= 0))
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

strSecs ∷ String → Maybe Instant
strSecs = toInstant <∘> Number.fromString

parseLine ∷ String → Maybe Score
parseLine a = join (match <$> reg <@> a) <#> toArray >>= tail >>= case _ of
  [s, m, d, o] -> score <$> (s >>= Int.fromString) 
                        <*> (m <#> modeFromString)
                        <*> (d >>= strSecs)
                        <*> o
  _ -> Nothing
  where 
    reg = hush $ regex "^([^ ]*) ([^ ]*) ([^ ]*) (.*)$" mempty

type File = { scores ∷ Map Mode (Array Score), lastUpdated ∷ Instant }

parseFile ∷ String → Maybe File
parseFile file = do
  {head, tail} ← uncons $ split (Pattern "\n") file
  lastUpdated ← strSecs head
  let scores = Map.fromFoldableWith append $ lift2 Tuple _.mode pure <$> mapMaybe parseLine tail
  pure {scores, lastUpdated}

main ∷ Effect Unit
main = HA.runHalogenAff do
  file ← AJW.get string "hist.txt"
       >>= liftEither ∘ lmap (error ∘ AJ.printError)
  datas ← liftEither $ note (error "file failed parsing") $ parseFile file.body
  body ← HA.awaitBody
  runUI component datas body

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
    , HP.title $ owner<>" "<> show score <>" in "<> show mode <>" at "<> formatTime date]
    [ HH.text $ show score
    , HH.small_ [HH.text $ " " <> show mode]
    , HH.br_
    , HH.small_ [HH.text owner]
    ]

search ∷ ∀i. (i → Boolean) → Array i → Int
search cmp arr = search_ cmp arr 0 (length arr)
  where 
    search_ cmp arr lo hi
      | lo >= hi  = lo
      | otherwise =
        let mid = (lo + hi) `div` 2 in
        case cmp <$> arr !! mid of
          Just true  → search_ cmp arr (mid+1) hi
          Just false → search_ cmp arr lo mid
          Nothing    → hi

displayScore ∷ ∀w i. Map Mode (Array Score) → Instant → Mode → HH.HTML w i
displayScore scores time mode = makeCell mode do
  arr ← Map.lookup mode scores
  arr !! search (\y→ y.date > time) arr

data Action =
    Increment
  | ToggleScol
  | ToggleShowEmpty
  | ChangeTime String
  | ChangeTimeBy Number
  | ChangeModes String
  | ResetModes

type State =
  { scores ∷ Map Mode (Array Score)
  , lastUpdated ∷ Instant
  , modes  ∷ Array Mode
  , scol   ∷ Boolean
  , showEmpty ∷ Boolean
  , time   ∷ Instant
  }

rotate ∷ ∀a. Array a → Array a
rotate arr = case unsnoc arr of
  Just { init, last } → [last] <> init
  Nothing -> []

initialState ∷ File → State
initialState {scores,lastUpdated} = 
  { scores
  , lastUpdated
  , time:      lastUpdated
  , modes:     allModes
  , scol:      false
  , showEmpty: true
  }

formatTime ∷ Instant → String
formatTime = fromRight "1970-01-01T00:00:00" ∘ formatDateTime "YYYY-MM-DDTHH:mm:ss" ∘ toDateTime

toInstant ∷ Number → Instant
toInstant = unsafePartial $
  fromJust ∘ instant ∘ on clamp unInstant bottom top ∘ Milliseconds ∘ (*) 1000.0

handleAction ∷ ∀o m. MonadEffect m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment → (H.get >>= log ∘ show) $> unit
  ToggleScol      → H.modify_ (\x→ x {scol      = not x.scol     })
  ToggleShowEmpty → H.modify_ (\x→ x {showEmpty = not x.showEmpty})
  ChangeModes s   → H.modify_ _ {modes = modeFromString <$> split (Pattern " ") s}
  ResetModes      → H.modify_ _ {modes = allModes}
  ChangeTime s    → applyWhen (S.length s <= 16) (_<>":00") s 
                    # unformatDateTime "YYYY-MM-DDTHH:mm:ss"
                    # hush <#> fromDateTime 
                    # maybe (pure unit) (\y→ H.modify_ _ {time = y})
  ChangeTimeBy n  → H.modify_ (\x → x {time = toInstant $ n + unwrap (unInstant x.time) / 1000.0 })

_cons ∷ ∀a b. Prism (Array a) (Array b) (a /\ Array a) (b /\ Array b)
_cons = prism (\(a/\b)→[a]<>b) $ note [] ∘ (\{head,tail}→head/\tail) <∘> uncons

addHeaders ∷ ∀i w. State → Array (Array (HH.HTML i w)) → Array (Array (HH.HTML i w))
addHeaders {scol, showEmpty, modes} arr =
  if scol then arr # sel %~ zipWith (flip snoc ∘ head "diag") modes
          else arr # sel %~ zipWith (cons      ∘ head "left") modes
                 # applyWhen showEmpty (ix 0 %~ cons (HH.th_ []))
  where sel = if showEmpty then _cons∘_2 else identity
        head c x = HH.th [HP.class_ $ H.ClassName c] [HH.text $ show x] 

renderTable ∷ ∀m. State → H.ComponentHTML Action () m
renderTable state@{scores, time} =
  HH.table_ $ map HH.tr_ $ addHeaders state $ map (displayScore scores time) <$> table state

render ∷ ∀m. State → H.ComponentHTML Action () m
render state = 
  HH.div_
    [ renderTable state
    , HH.button [HE.onClick \_→Increment] [HH.text "bee"]
    , HH.br_
    , HH.label_
      [ HH.input [HP.type_ HP.InputCheckbox, HE.onClick \_→ToggleScol]
      , HH.text "single column one line"
      ]
    , HH.br_
    , HH.label_
      [ HH.input [HP.type_ HP.InputCheckbox, HE.onClick \_→ToggleShowEmpty]
      , HH.text "show empty score (ε)"
      ]
    , HH.br_
    , HH.label_
      [ HH.text "modes: "
      , HH.input 
        [ HP.type_ HP.InputText
        , HP.class_ (H.ClassName "modes")
        , HP.value $ joinWith " " $ modeToString <$> state.modes
        , HE.onValueChange ChangeModes
        ]
      ]
    , HH.button [HE.onClick \_→ResetModes] [HH.text "reset"]
    , HH.br_

    , HH.button [HE.onClick \_→ChangeTimeBy $ -365.0*86400.0 ] [ HH.text "-y" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $  -30.0*86400.0 ] [ HH.text "-30d" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $       -86400.0 ] [ HH.text "-d" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $        -3600.0 ] [ HH.text "-h" ]
    , HH.input 
      [ HP.type_ HP.InputDatetimeLocal
      , HP.value $ formatTime state.time
      , HE.onValueChange ChangeTime
      , HP.attr (H.AttrName "step") "1"
      , HP.attr (H.AttrName "max") $ formatTime state.lastUpdated
      ]
    , HH.button [HE.onClick \_→ChangeTimeBy $         3600.0 ] [ HH.text "+h" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $        86400.0 ] [ HH.text "+d" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $   30.0*86400.0 ] [ HH.text "+30d" ]
    , HH.button [HE.onClick \_→ChangeTimeBy $  365.0*86400.0 ] [ HH.text "+y" ]
    ]

component ∷ ∀query o m. MonadEffect m ⇒ H.Component query File o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
