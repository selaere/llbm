-- html stuff
module Main.Thorax where

import Prelude

import DOM.HTML.Indexed as DOM
import Data.Array (cons, elemIndex, filter, mapWithIndex, snoc, updateAt, zipWith)
import Data.DateTime.Instant (diff, unInstant)
import Data.Foldable (any, fold)
import Data.HashMap as HM
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid as Monoid
import Data.Newtype (un, unwrap)
import Data.Number as Number
import Data.String as S
import Data.Time.Duration (Seconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Main.Common (bool, doWhen, (<<#>>), (∘), (≡), (≢), (⋄), (⍪))
import Main.JsStuff (murmur3, formatTime, showTime)
import Main.Mode (Mode, ε, (∩))
import Main.PointerEvents as PE
import Main.Spine (Action(..), Cell(..), Coloring(..), Leaderboard(..), ScoreWith, Selection(..), State, contextify, findScore, leaderboard, period)
import Web.Event.Event (EventType(..))


hsl ∷ Int → Int → Int → String
hsl h s l = "background-color:hsl("⋄show h⋄","⋄show s⋄"%,"⋄show l⋄"%)"

colorName ∷ Int → String → String
--color "☭🐝" = "background-color:rgb(198,234,169)"
colorName seed name = hsl (h `mod` 360) 60 ((h `div` 360 `mod` 45 * 30) `div` 40 + 55)
  where h = murmur3 name seed

color ∷ ∀r. Coloring → Int → ScoreWith r → String
color ByName seed score = colorName seed score.owner
color ByDate _    score = hsl hue 60 (Int.floor lig)
  where hue = (secs - 1577836800) `div` 500000 `max` -20
        lig = 95.0 - Number.log (Int.toNumber $ score.score + 5) * 45.0 / Number.log 1005.0
        secs = Int.floor (unwrap (unInstant score.date) / 1000.0)

-- this is written in a strange way bc we dont want this to be uncurried
-- (i dont want to call findScore once for every cell)
selectionClass ∷ State → Cell → String
selectionClass {selection: SelectNothing} = \_→"on"
selectionClass {context, modes, selection: SelectRow m, selectHard} =
  case _ of Filled {mode}  → go mode
            Unclaimed mode → go mode
            Header _ mode  → if m ≡ mode && selectHard then "sel" else "on"
            Empty          → "off"
  where go = bool "off" "on" ∘ if m ≡ ε then \x→ any (\y→y ⋄ context ≡ x) modes
                                        else \x→ x ∩ m ≢ context
selectionClass {scores, time, selection: SelectMode m, selectHard} =
  case findScore scores time m of
    Just {owner, mode} → case _ of
      Filled {owner:owner'} | owner ≢ owner' → "off"
      Filled {mode: mode' } | mode  ≡ mode'  → bool "hover" "sel" selectHard
      _ → "on"
    _ → \_→"on"

pointerAttrs
  ∷ ∀r. Selection → Action → Array (HH.IProp
    ( onPointerDown  ∷ PE.PointerEvent
    , onPointerEnter ∷ PE.PointerEvent
    , onPointerLeave ∷ PE.PointerEvent
    , onPointerMove  ∷ PE.PointerEvent
    , onPointerUp    ∷ PE.PointerEvent
    | r )
    Action )
pointerAttrs sel along =
  [ PE.onPointerEnter\_→Select sel
  , PE.onPointerLeave\_→Select SelectNothing
  , PE.onPointerDown  $ Down along
  , PE.onPointerUp    $ Up (SelectHard sel) along
  , PE.onPointerMove \_→Move
  , HE.handler (EventType "contextmenu") PreventDefault
  ]

makeCell' ∷ ∀w. Mode → HH.Node DOM.HTMLtd w Action
makeCell' mode a = HH.td (a ⋄ pointerAttrs (SelectMode mode) (Goto mode))

makeCell ∷ ∀w. State → String → Cell → HH.HTML w Action
makeCell _     sel (Header s mode) = HH.th
  ( [HP.classes $ [HH.ClassName s, HH.ClassName sel]]
  ⋄ pointerAttrs (SelectRow mode) (AddContext mode) )
  [HH.text $ show mode]
makeCell _     _    Empty = HH.td [] []
makeCell _     sel (Unclaimed mode) = makeCell' mode [classic sel] [HH.text $ show mode]
makeCell state sel (Filled s) = makeCell' s.mode
  [ HP.style 
    $ color state.coloring state.seed s
    # let t = (un Seconds $ diff s.date state.time) / (state.speed * 3600.0 * period) in
      doWhen (isJust state.timerSid && t > -16.0 )
             (\x→x ⋄ ";outline:2px rgba(255,200,0,"⋄ show (Int.floor $ 256.0 + t * 16.0) ⋄"%) solid")
  , HP.title $ s.owner⋄" "⋄ show s.score ⋄" in "⋄ show s.mode ⋄" at "⋄ showTime s.date
  , HP.classes $ doWhen s.stricken (_⋄[H.ClassName "stricken"]) [ H.ClassName sel ]
  ]
  [ HH.text $ show s.score
  , HH.small_ [HH.text $ " " ⋄ show s.mode]
  , HH.br_
  , HH.small_ [HH.text s.owner]
  ]

classic ∷ ∀r i. String → HH.IProp (class ∷ String | r) i
classic = HP.class_ ∘ H.ClassName

history ∷ ∀w i. State → Mode → HH.HTML w i
history state mode =
  fold (HM.lookup mode state.scores)
  <#> (\{score,date,owner} → HH.tr
    (if date > state.time then [classic "dark"] else [])
    [ HH.td [HP.style $ colorName state.seed owner] [HH.text $ owner]
    , HH.td_ [HH.text $ show score]
    , HH.td_ [HH.text $ showTime date]
    ])
  # cons (HH.tr_ $ HH.th_ ∘ pure ∘ HH.text <$> ["name","points","date"])
  # HH.table [classic "scores"]

renderTable ∷ ∀w. State → Array (Array Cell) → HH.HTML w Action
renderTable state tab = tab
  # addHeaders state
  <<#>> (\m→ makeCell state (selectionClass state m) m)
  <#> HH.tr_
  # HH.table [classic "y"]

addHeaders ∷ State → Array (Array Cell) → Array (Array Cell)
addHeaders {scol, showEmpty, modes, funModes} =
  zipWith add $ doWhen (showEmpty || funModes ≡ 4) (cons ε) modes
    where add x | funModes≡4 = \y→fromMaybe y (elemIndex Empty y >>=
                                              \i→updateAt i (Header "" x) y)
                | scol       = flip snoc $ Header (if x ≡ ε then "right" else "diag") x
                | otherwise  = cons $ Header "left" x
          --select x = if selection ≡ SelectRow x && selectHard then "sel" else "on"

renderLeaderboard ∷ ∀w i. State → Leaderboard → HH.HTML w i
renderLeaderboard state (Leaderboard {scores,unclaimed,ignored,lb}) =
  HH.div_ [HH.text label, table]
  where
    label = show scores⋄" scores, "⋄show unclaimed⋄" unclaimed, "⋄show ignored⋄" ignored"
    selOwner = case state.selection of
      SelectMode m → _.owner <$> findScore state.scores state.time m
      _ → Nothing
    table = mapWithIndex (\i (name⍪no⍪score) →
      HH.tr
      ( Monoid.guard ( any (_ ≡ name) selOwner) [classic "selowner"] )
      [ HH.td_ [HH.text $ show (i+1) ⋄ "."]
      , HH.td [HP.style $ colorName state.seed name] [HH.text name]
      , HH.td_ [HH.text $ show no]
      , HH.td_ [HH.text $ show score]
      ]) lb
      # cons (HH.tr_ $ HH.th_ ∘ pure ∘ HH.text <$> ["rank","name","high scores","total points"])
      # HH.table [classic "scores"]

render ∷ ∀w. State → HH.HTML w Action
render state =
  HH.div_ $ flip append [HH.main_ [ renderTable (contextify state) state.mTab ]] [HH.nav_
    [ HH.h2_ [HH.text ",leader lead board man? (llbm)"]
    , HH.p_ [HH.text $
      "hover or right click or tap on a score to see all historical high scores. left click or long tap on a score to play. left click or long tap on one of the headers in the "⋄bool "diagonal" "left" state.scol⋄" to see more."]
    , if state.context ≢ ε then HH.p_
      [ HH.text "viewing modes "
      , HH.b_ [HH.text $ show state.context]
      , HH.text ". "
      , HH.button [ HE.onClick \_→ResetContext ] [HH.text "reset"]
      ] else HH.text ""
    , details [HH.text "table settings"]
      [ labeled "" "diagonal headers"
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.scol
        , HE.onClick \_→ToggleScol]
      , HH.br_
      , labeled "" "show empty score"
        [ HP.type_ HP.InputCheckbox
        , HP.checked state.showEmpty
        , HE.onClick \_→ToggleShowEmpty]
      , HH.br_
      , HH.div_
        [ HH.text "color cells"
        , labeled "" "by name"
          [ HP.type_ HP.InputRadio
          , HP.id "coloring"
          , HP.checked $ state.coloring ≡ ByName
          , HE.onClick \_→ChangeColoring ByName
          ]
        , labeled "" "by date and score"
          [ HP.type_ HP.InputRadio
          , HP.id "coloring"
          , HP.checked $ state.coloring ≡ ByDate
          , HE.onClick \_→ChangeColoring ByDate
          ]
        ]
      , labeled "color seed: " ""
        [ HP.type_ HP.InputNumber
        , HP.value $ show state.seed
        , HE.onValueChange ChangeSeed
        ]
      , HH.br_
      , labeled "modes: " ""
        [ HP.type_ HP.InputText
        , classic "modes"
        , HP.value ∘ S.joinWith " " $ show <$> state.modes
        , HE.onValueChange ChangeModes
        ]
      , HH.button [HE.onClick \_→ResetModes] [HH.text "reset"]
      , HH.br_
      , labeled "ignored modes: " ""
        [ HP.type_ HP.InputText
        , HP.placeholder "t md"
        , HP.value ∘ S.joinWith " " $ show <$> state.ignoredModes
        , HE.onValueChange IgnoreModes
        ]
        , HH.div_ let 
            b x n = labeled "" x
              [ HP.type_ HP.InputRadio
              , HP.id "funmodes"
              , HP.checked $ state.funModes ≡ n
              , HE.onClick \_→ChangeFunModes n
              ]
          in [ HH.text "fun modes:", b "bw" 0, b "bW" 1, b "Bw" 2, b "BW" 3, b "bw\\BW" 4 ]
      ]
    , details [HH.text "history"]
      [ HH.div [classic "small"] [ HH.text $ " scores were last updated at "⋄ showTime state.lastUpdated ⋄". all times are in UTC. dates prepended with ≈ are approximate." ]
      , HH.button [HE.onClick \_→SkipBackward] [HH.text "⏮"]
      , labeled " date: " ""
        [ HP.type_ HP.InputDatetimeLocal
        , HP.value $ formatTime state.time
        , HE.onValueChange ChangeTime
        , HP.attr (H.AttrName "step") "1"
        , HP.attr (H.AttrName "max") $ formatTime state.lastUpdated
        ]
      , HH.button [HE.onClick \_→SkipForward] [HH.text "⏭"]
      , HH.br_
      , skip (-365*86400) "-y" , skip ( -30*86400) "-30d", skip (  -7*86400) "-7d"
      , skip (    -86400) "-d" , skip (     -3600) "-h"
      , HH.text " ⏪\xFE0E time travel ⏩\xFE0E "
      , skip (      3600) "+h" , skip (     86400) "+d"
      , skip (   7*86400) "+7d", skip (  30*86400) "+30d", skip ( 365*86400) "+y"
      , HH.br_
      , HH.button 
        [HE.onClick \_→StartTimer]
        [HH.text if isNothing state.timerSid then "start" else "stop"]
      , labeled " timelapse at " " h⋅s⁻¹"
        [ HP.type_ HP.InputNumber
        , HP.value $ show state.speed
        , HE.onValueChange ChangeSpeed
        , classic "speed"
        ]
      ]
    , HH.br_
    , case state.selection of
        SelectMode m → HH.div_ 
          [ HH.h3_ [ HH.text $ "high score history for mode "⋄ show m ]
          , history state m ]
        SelectRow m → HH.div_
          [ HH.h3_ [ HH.text $ "leaderboard for row "⋄ show m]
          , renderLeaderboard state
            $ leaderboard $ filter (\x→selectionClass state x ≢ "off") $ join state.mTab ]
        SelectNothing → HH.text ""
    , HH.h3_ [ HH.text "leaderboard for current table" ]
    , renderLeaderboard state state.mLeaderboard
    ]]
  where skip n t = HH.button [HE.onClick \_→ChangeTimeBy $ Int.toNumber n ] [ HH.text t ]
        labeled pre post props = HH.label_ [HH.text pre, HH.input props, HH.text post]
        details s x = HH.details [HP.attr (H.AttrName "open") ""] [HH.summary_ s, HH.div_ x]
