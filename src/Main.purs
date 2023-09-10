module Main where

import Prelude

import Affjax as AJ
import Affjax.ResponseFormat (string)
import Affjax.Web as AJW
import Control.Biapply (bilift2)
import Control.Monad.Error.Class (liftEither)
import Data.Array (cons, filter, foldl, length, mapMaybe, mapWithIndex, snoc, sortBy, tail, take, uncons, unsnoc, zipWith, (!!), (..))
import Data.Array.NonEmpty (toArray)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant, fromDateTime, instant, toDateTime, unInstant)
import Data.Either (Either(..), fromRight, hush, note)
import Data.Foldable (fold)
import Data.Formatter.DateTime (formatDateTime, unformatDateTime)
import Data.Function (on)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.String (Pattern(..), joinWith)
import Data.String as S
import Data.String.Common (split)
import Data.String.Regex (regex, match)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (swap)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Main.Common (doWhen, (∘), (≡), (≢), (⋄), (⍪))
import Main.Mode (Mode, ε, (∩))
import Main.Mode as Mode
import Murmur3 (hash)
import Partial.Unsafe (unsafePartial)
import DOM.HTML.Indexed as DOM

type Score =
  { score ∷ Int
  , mode  ∷ Mode
  , date  ∷ Instant
  , owner ∷ String }

score ∷ Int → Mode → Instant → String → Score
score = {score: _, mode: _, date: _, owner: _}

parseLine ∷ String → Maybe Score
parseLine a = join (match <$> reg <@> a) <#> toArray >>= tail >>= case _ of
  [s, m, d, o] -> score <$> (s >>= Int.fromString)
                        <*> (m <#> Mode.fromString)
                        <*> (d >>= Number.fromString <#> toInstant)
                        <*> o
  _ -> Nothing
  where
    reg = hush $ regex "^([^ ]*) ([^ ]*) ([^ ]*) (.*)$" mempty

type File = { scores ∷ HashMap Mode (Array Score), lastUpdated ∷ Instant }

parseFile ∷ String → Maybe File
parseFile file = do
  {head, tail} ← uncons $ split (Pattern "\n") file
  lastUpdated ← toInstant <$> Number.fromString head
  let scores = foldl (\m i→HM.insertWith (flip (⋄)) i.mode [i] m) mempty $ mapMaybe parseLine tail
  pure {scores, lastUpdated}

main ∷ Effect Unit
main = HA.runHalogenAff do
  file ← AJW.get string "hist.txt"
       >>= liftEither ∘ lmap (error ∘ AJ.printError)
  datas ← liftEither $ note (error "file failed parsing") $ parseFile file.body
  body ← HA.awaitBody
  runUI component datas body

table ∷ State → Array (Array (Either Mode Score))
table { modes, scol, showEmpty, context, scores, time } =
  1 .. (length modes)
  <#> take `flip` modes
  <#> doWhen scol rotate
  # zipWith (map ∘ append) modes
  # doWhen showEmpty (append [[context]])
  <#> map (findScore scores time)

leaderboard ∷ ∀w i. Array (Array (Either Mode Score)) → HH.HTML w i
leaderboard tab =
  hush `mapMaybe` join tab
  # foldl (\m i→HM.insertWith (join bilift2 (+)) i.owner (1 ⍪ max 0 i.score) m) HM.empty
  # HM.toArrayBy (⍪)
  # sortBy (on (flip compare) swap)
  # mapWithIndex (\i (name⍪no⍪score) → HH.tr_
    [ HH.td_ [HH.text $ show (i+1) ⋄ "."]
    , HH.td [HP.style $ color name] [HH.text name]
    , HH.td_ [HH.text $ show no]
    , HH.td_ [HH.text $ show score]
    ])
  # cons (HH.tr_ $ HH.th_ ∘ pure ∘ HH.text <$> ["rank","name","high scores","total points"])
  # HH.table [ HP.class_ $ H.ClassName "scores" ]

history ∷ ∀w i. State → Mode → HH.HTML w i
history state mode =
  fold (HM.lookup mode state.scores)
  <#> (\{score,date,owner} → HH.tr
    (if date > state.time then [HP.class_ $ H.ClassName "dark"] else [])
    [ HH.td [HP.style $ color owner] [HH.text $ owner]
    , HH.td_ [HH.text $ show score]
    , HH.td_ [HH.text $ showTime date]
    ])
  # cons (HH.tr_ $ HH.th_ ∘ pure ∘ HH.text <$> ["name","points","date"])
  # HH.table [ HP.class_ $ H.ClassName "scores" ]

color ∷ String → String
color "☭🐝" = "background-color:rgb(198,234,169)" -- temporary (elm Murmur3 and ursi/purescript-murmur3 treat unicode differently)
color name = "background-color:hsl("⋄ show hue ⋄",60%,"⋄ show lgt ⋄"%"
  where h = hash 3054 name
        hue = h `mod` 360
        lgt = ((h `div` 360) `mod` 45) + 40

makeCell' ∷ ∀w. Mode → HH.Node DOM.HTMLtd w Action
makeCell' mode a = 
  HH.td (a ⋄ [HE.onMouseEnter \_→Hover mode, HE.onMouseLeave \_→Unhover])
  ∘ pure
  ∘ HH.a [HP.href $ "https://ubq323.website/ffbm#" ⋄ show mode]

makeCell ∷ ∀w. Either Mode Score → HH.HTML w Action
makeCell (Left mode) = makeCell' mode [] [HH.text $ show mode]
makeCell (Right {mode, score, owner, date}) = makeCell' mode
  [ HP.style $ color owner
  , HP.title $ owner⋄" "⋄ show score ⋄" in "⋄ show mode ⋄" at "⋄ showTime date
  ]
  [ HH.text $ show score
  , HH.small_ [HH.text $ " " ⋄ show mode]
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

findScore ∷ HashMap Mode (Array Score) → Instant → Mode → Either Mode Score
findScore scores time mode = note mode do
  arr ← HM.lookup mode scores
  arr !! search (\y→ y.date > time) arr

data Action =
    ToggleScol
  | ToggleShowEmpty
  | ChangeTime String
  | ChangeTimeBy Number
  | ResetTime
  | ChangeModes String
  | ResetModes
  | AddContext Mode
  | ResetContext
  | Hover Mode
  | Unhover

type State =
  { scores    ∷ HashMap Mode (Array Score)
  , lastUpdated ∷ Instant
  , time      ∷ Instant
  , context   ∷ Mode
  , modes     ∷ Array Mode
  , scol      ∷ Boolean
  , showEmpty ∷ Boolean
  , selection ∷ Maybe Mode
  }

rotate ∷ ∀a. Array a → Array a
rotate arr = case unsnoc arr of
  Just { init, last } → [last] ⋄ init
  Nothing -> []

initialState ∷ File → State
initialState {scores,lastUpdated} =
  { scores
  , lastUpdated
  , context:   ε
  , time:      lastUpdated
  , modes:     Mode.all
  , scol:      false
  , showEmpty: true
  , selection: Nothing
  }

formatTime ∷ Instant → String
formatTime = fromRight "1970-01-01T00:00:00" ∘ formatDateTime "YYYY-MM-DDTHH:mm:ss" ∘ toDateTime

showTime ∷ Instant → String
showTime = fromRight "1970-01-01 00:00:00" ∘ formatDateTime "YYYY-MM-DD HH:mm:ss" ∘ toDateTime

toInstant ∷ Number → Instant
toInstant = unsafePartial $
  fromJust ∘ instant ∘ on clamp unInstant bottom top ∘ Milliseconds ∘ (*) 1000.0

handleAction ∷ ∀o m. MonadEffect m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  ToggleScol      → H.modify_ \x→ x {scol      = not x.scol     }
  ToggleShowEmpty → H.modify_ \x→ x {showEmpty = not x.showEmpty}
  ChangeModes s   → H.modify_ _ {modes = Mode.fromString <$> split (Pattern " ") s}
  ResetModes      → H.modify_ _ {modes = Mode.all}
  ChangeTime s    → doWhen (S.length s <= 16) (_⋄":00") s
                    # unformatDateTime "YYYY-MM-DDTHH:mm:ss"
                    # hush <#> fromDateTime
                    # maybe (pure unit) (\y→ H.modify_ _ {time = y})
  ChangeTimeBy n  → H.modify_ \x→ x {time = toInstant $ n + unwrap (unInstant x.time) / 1000.0 }
  ResetTime       → H.modify_ \x→ x {time = x.lastUpdated}
  AddContext m    → H.modify_ \x→ x {context = doWhen (m ≢ ε) (append x.context) m}
  ResetContext    → H.modify_ _ {context = ε}
  Hover m         → H.modify_ _ {selection = Just m}
  Unhover         → H.modify_ _ {selection = Nothing}

addHeaders ∷ ∀w. State → Array (Array (HH.HTML w Action)) → Array (Array (HH.HTML w Action))
addHeaders {scol, showEmpty, modes} =
  zipWith add $ doWhen showEmpty (cons ε) modes
    where add x = if scol then flip snoc $ head (if x ≡ ε then "right" else "diag") x
                          else cons $ head "left" x
          head c x = HH.th [HP.class_ $ H.ClassName c, HE.onClick \_→AddContext x] [HH.text $ show x]

renderTable ∷ ∀w. Array (Array (Either Mode Score)) → State → HH.HTML w Action
renderTable tab state =
  HH.table [HP.class_ $ H.ClassName "y"] $
    map HH.tr_ $ addHeaders state $ map makeCell <$> tab

contextifyState ∷ State → State
contextifyState state@{context,modes} = state { modes =
  append context <$> filter (\x→ x ∩ context ≡ ε) modes }

render ∷ ∀w. State → HH.HTML w Action
render state =
  HH.div_ $ flip append [HH.main_ [ renderTable tab $ contextifyState state ]] [HH.nav_
    [ HH.h2_ [HH.text ",leader lead board man? (llbm)"]
    , HH.p_ [HH.text $ "click on a score to play. click on a gamemode to see more. scores last updated "⋄ showTime state.lastUpdated ⋄" (UTC+00:00)."]
    , if state.context ≢ ε then HH.p_
      [ HH.text "viewing modes "
      , HH.b_ [HH.text $ show state.context]
      , HH.text ". "
      , HH.button [ HE.onClick \_→ResetContext ] [HH.text "reset"]
      ] else HH.text ""
    , HH.label_
      [ HH.input [ HP.type_ HP.InputCheckbox
                 , HP.checked state.scol
                 , HE.onClick \_→ToggleScol]
      , HH.text "single column one line"
      ]
    , HH.br_
    , HH.label_
      [ HH.input [ HP.type_ HP.InputCheckbox
                 , HP.checked state.showEmpty
                 , HE.onClick \_→ToggleShowEmpty]
      , HH.text "show empty score (ε)"
      ]
    , HH.br_
    , HH.label_
      [ HH.text "modes: "
      , HH.input
        [ HP.type_ HP.InputText
        , HP.class_ (H.ClassName "modes")
        , HP.value $ joinWith " " $ show <$> state.modes
        , HE.onValueChange ChangeModes
        ]
      ]
    , HH.button [HE.onClick \_→ResetModes] [HH.text "reset"]
    , HH.br_
    , HH.label_
      [ HH.text "date: "
      , HH.input
        [ HP.type_ HP.InputDatetimeLocal
        , HP.value $ formatTime state.time
        , HE.onValueChange ChangeTime
        , HP.attr (H.AttrName "step") "1"
        , HP.attr (H.AttrName "max") $ formatTime state.lastUpdated
        ]
      ]
    , HH.button [HE.onClick \_→ResetTime] [HH.text "skip forward"]
    , HH.br_
    , skip (-365*86400) "-y" , skip ( -30*86400) "-30d", skip (  -7*86400) "-7d"
    , skip (    -86400) "-d" , skip (     -3600) "-h"
    , skip (      3600) "+h" , skip (     86400) "+d"
    , skip (   7*86400) "+7d", skip (  30*86400) "+30d", skip ( 365*86400) "+y"
    , case state.selection of
        Just m → HH.div_ 
          [ HH.h3_ [ HH.text $ "high score history for mode "⋄ show m ]
          , history state m ]
        Nothing → HH.text ""
    , HH.h3_ [ HH.text "leaderboard for current table" ]
    , leaderboard tab
    ]]
  where tab = table $ contextifyState state
        skip n t = HH.button [HE.onClick \_→ChangeTimeBy $ Int.toNumber n ] [ HH.text t ]

component ∷ ∀query o m. MonadEffect m ⇒ H.Component query File o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
