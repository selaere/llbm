module Main.Spine where

import Prelude

import Control.Biapply (bilift2)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State as St
import Data.Array (filter, foldl, length, mapMaybe, modifyAt, sortBy, tail, take, transpose, uncons, unsnoc, zipWith, (!!), (..))
import Data.Array.NonEmpty (toArray)
import Data.DateTime.Instant (Instant, fromDateTime, instant, unInstant)
import Data.Either (hush)
import Data.Foldable (any, sum, traverse_)
import Data.Formatter.DateTime (unformatDateTime)
import Data.Function (on)
import Data.Functor.Compose (Compose(..))
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.HashSet as HSet
import Data.Int as Int
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Data.Newtype (under, unwrap)
import Data.Number as Number
import Data.String as S
import Data.String.Regex as RE
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Subscription as HS
import Main.Common (type (⍪), bool, doWhen, (<<#>>), (<∘>), (∘), (≡), (≢), (≤), (≥), (⋄), (⍪))
import Main.Mode (Mode, ε, (∩))
import Main.Mode as Mode
import Partial.Unsafe (unsafePartial)
import Record as Record
import Web.Event.Event (Event, preventDefault)
import Web.HTML (window)
import Web.HTML.Window (open)
import Web.PointerEvent.PointerEvent as PtrE
import Web.UIEvent.MouseEvent as MouseEvent

type Score  = ScoreWith ()
type ScoreS = ScoreWith (stricken ∷ Boolean)

type ScoreWith r = 
  { score ∷ Int
  , mode  ∷ Mode
  , date  ∷ Instant
  , owner ∷ String
  | r }

--data CellWith r = Filled (ScoreWith r) | Unclaimed Mode | Empty | Header String Mode

data Cell = Filled (ScoreWith (stricken ∷ Boolean)) | Unclaimed Mode | Empty | Header String Mode
derive instance Eq Cell

--derive instance Eq Cell

parseLine ∷ String → Maybe Score
parseLine a = join (RE.match <$> reg <@> a) <#> toArray >>= tail >>= case _ of
  [s, m, d, o] → score <$> (s >>= Int.fromString)
                       <*> (m <#> Mode.fromString)
                       <*> (d >>= Number.fromString <#> toInstant)
                       <*> o
  _ → Nothing
  where score = {score: _, mode: _, date: _, owner: _}
        reg = hush $ RE.regex "^([^ ]*) ([^ ]*) ([^ ]*) (.*)$" mempty

type File = { scores ∷ HashMap Mode (Array Score), lastUpdated ∷ Instant }

parseFile ∷ String → Maybe File
parseFile file = do
  {head, tail} ← uncons $ S.split (S.Pattern "\n") file
  lastUpdated ← toInstant <$> Number.fromString head
  let scores = foldl (\m i→HM.insertWith (flip (⋄)) i.mode [i] m) mempty $ mapMaybe parseLine tail
  pure {scores, lastUpdated}

table ∷ State → Array (Array Cell)
table s@{funModes: 4} =
  zipWith (⋄)
  (doWhen (not s.showEmpty) ([[]]⋄_) $ table s {funModes=0})
  (doWhen s.showEmpty go ∘ ([Empty]⋄_) <∘> (_⋄[[]]) ∘ transpose $ table s {funModes=3,showEmpty=false})
  where go = unsafePartial $ fromJust ∘ modifyAt 0 (fromJust ∘ tail)
table { modes, scol, showEmpty, context, scores, time, funModes } =
  1 .. length modes
  <#> (take <@> funWhen 2 modes)
  <#> doWhen scol rotate
  # zipWith (map ∘ append) (funWhen 1 modes)
  # doWhen showEmpty (append [[context]])
  <<#>> findCell scores time
    where funWhen n = doWhen (funModes.&.n≢0) (map Mode.fun)

asFilled ∷ Cell → Maybe ScoreS
asFilled (Filled x) = Just x
asFilled _          = Nothing

newtype Leaderboard = Leaderboard
  { scores ∷ Int, unclaimed ∷ Int, ignored ∷ Int, lb ∷ Array (String⍪Int⍪Int) }
leaderboard ∷ Array Cell → Leaderboard
leaderboard tab = Leaderboard
  { lb: filter (not ∘ _.stricken) tabs
    # foldl (\m i→HM.insertWith (join bilift2 (+)) i.owner (1⍪ max 0 i.score) m) HM.empty
    # HM.toArrayBy (⍪)
    # sortBy (flip compare `on` swap)
  , scores:    sum $ bool 1 0 ∘ _.stricken  <$> tabs
  , unclaimed: sum $ bool 0 1 ∘ isUnclaimed <$> tab
  , ignored:   sum $ bool 0 1 ∘ _.stricken  <$> tabs }
  where tabs = mapMaybe asFilled tab
        isUnclaimed (Unclaimed _) = true
        isUnclaimed _ = false

search ∷ ∀i. (i → Boolean) → Array i → Int
search cmp arr = search_ cmp arr 0 (length arr)
  where
    search_ cmp arr lo hi
      | lo ≥ hi   = lo
      | otherwise =
        let mid = (lo + hi) `div` 2 in
        case cmp <$> arr !! mid of
          Just true  → search_ cmp arr (mid+1) hi
          Just false → search_ cmp arr lo mid
          Nothing    → hi

findScore ∷ HashMap Mode (Array Score) → Instant → Mode → Maybe Score
findScore scores time mode = do
  arr ← HM.lookup mode scores
  arr !! search (\y→ y.date > time) arr

findCell ∷ HashMap Mode (Array Score) → Instant → Mode → Cell
findCell s t m = maybe (Unclaimed m) (Filled ∘ Record.merge { stricken: false }) (findScore s t m)

data Action =
    ToggleScol
  | ToggleShowEmpty
  | ChangeTime String
  | ChangeTimeBy Number
  | SkipBackward
  | SkipForward
  | ChangeModes String
  | IgnoreModes String
  | ResetModes
  | AddContext Mode
  | ResetContext
  | Select Selection
  | SelectHard Selection
  | Goto Mode
  | ChangeSpeed String
  | StartTimer
  | Tick
  | ChangeSeed String
  | ChangeColoring Coloring
  | ChangeFunModes Int
  | Down      Action PtrE.PointerEvent
  | Up Action Action PtrE.PointerEvent
  | Move
  | PreventDefault Event

type State =
  { scores       ∷ HashMap Mode (Array Score)
  , lastUpdated  ∷ Instant
  , time         ∷ Instant
  , context      ∷ Mode
  , modes        ∷ Array Mode
  , ignoredModes ∷ Array Mode
  , scol         ∷ Boolean
  , showEmpty    ∷ Boolean
  , selection    ∷ Selection
  , selectHard   ∷ Boolean
  , timerSid     ∷ Maybe H.SubscriptionId
  , tapSid       ∷ Maybe H.SubscriptionId
  , speed        ∷ Number
  , seed         ∷ Int
  , coloring     ∷ Coloring
  , funModes     ∷ Int
  , mTab         ∷ Array (Array Cell)
  , mLeaderboard ∷ Leaderboard
  }

data Selection
  = SelectNothing
  | SelectMode Mode
  | SelectRow Mode
derive instance Eq Selection

rotate ∷ ∀a. Array a → Array a
rotate arr = case unsnoc arr of
  Just { init, last } → [last] ⋄ init
  Nothing → []

updateTab ∷ State → State
updateTab state = state {mTab = strike state.ignoredModes (table (contextify state)) }

-- this will be invoked by most actions, but NOT Select, so that we can have (hopefully) smoother hovering
updateLb ∷ State → State
updateLb = (\state→ state { mLeaderboard = leaderboard (join state.mTab) } ) ∘ updateTab

data Coloring = ByName | ByDate
derive instance Eq Coloring

initialState ∷ File → State
initialState {scores,lastUpdated} =
  updateLb
  { scores              , lastUpdated      , time:       lastUpdated
  , modes:     Mode.allB, ignoredModes: [] , context:    ε
  , scol:      false    , showEmpty: true  , selection:  SelectNothing
  , timerSid:  Nothing  , speed:     240.0 , selectHard: false
  , seed:      3054     , coloring:  ByName
  , mTab:      [[]]     , mLeaderboard: leaderboard [] -- these will be replaced immediately
  , tapSid:    Nothing  , funModes:  0
  }

toInstant ∷ Number → Instant
toInstant = unsafePartial $
  fromJust ∘ instant ∘ on clamp unInstant bottom top ∘ Milliseconds ∘ (*) 1000.0

advanceTime ∷ Number → Instant → Instant → Instant
advanceTime n now time = min now $ toInstant $ n + unwrap (unInstant time) / 1000.0

handleAction ∷ ∀o m. MonadAff m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Down along ev → case PtrE.pointerType ev of
    PtrE.Mouse → pure unit
    _ → whenM (isNothing <$> H.gets _.tapSid)
          (tapper along >>= H.subscribe >>= \sid → H.modify_ _ { tapSid = Just sid })
  Up ashort along ev → case PtrE.pointerType ev of
    PtrE.Mouse → case MouseEvent.button (PtrE.toMouseEvent ev) of
      2 → handleAction ashort
      0 → handleAction along
      _ → pure unit
    _ → H.gets _.tapSid >>= traverse_ \x→do
      H.unsubscribe x
      H.modify_ _ {tapSid = Nothing}
      handleAction ashort
  Move → H.gets _.tapSid >>= traverse_ \x→do
    H.unsubscribe x
    H.modify_ _ {tapSid = Nothing}
  PreventDefault ev → H.liftEffect (preventDefault ev)
  ToggleScol     → H.modify_ \x→ updateTab x {scol      = not x.scol     }
  ToggleShowEmpty→ H.modify_ \x→ updateLb  x {showEmpty = not x.showEmpty}
  ChangeModes s  → H.modify_ $ updateLb ∘ _ {modes = Mode.fromString <$> S.split (S.Pattern " ") s}
  IgnoreModes s  → H.modify_ $ updateLb ∘ _ {ignoredModes = filter (_ ≢ ε) $ Mode.fromString <$> S.split (S.Pattern " ") s}
  ResetModes     → H.modify_ $ updateLb ∘ _ {modes = Mode.allB}
  ChangeFunModes s→H.modify_ $ updateLb ∘ _ {funModes = s}
  ChangeTime s   → doWhen (S.length s ≤ 16) (_⋄":00") s
                   # unformatDateTime "YYYY-MM-DDTHH:mm:ss"
                   # hush <#> fromDateTime
                   # traverse_ \y→ H.modify_ $ updateLb ∘ _ {time = y}
  ChangeTimeBy n → H.modify_ \x→ updateLb x {time = advanceTime n x.lastUpdated x.time}
  SkipForward    → H.modify_ \x→ updateLb x {time = x.lastUpdated}
  SkipBackward   → H.modify_ \x→ updateLb x {time = toInstant 1602598380.0}
  AddContext m   → H.modify_ \x→ updateLb x {context = doWhen (m ≢ ε) (append x.context) m}
  ResetContext   → H.modify_ $ updateLb ∘ _ {context = ε}
  Select s       → unlessM (H.gets _.selectHard) (H.modify_ _ {selection = s})
  SelectHard s   → H.modify_ \x → if x.selection ≡ s && not x.selectHard
                     then x { selectHard = true , selection = s }
                     else x { selectHard = false, selection = SelectNothing }
  Goto m         → void ∘ H.liftEffect $
    window >>= open ("https://ubq323.website/ffbm/#"⋄show m) "_self" ""
  StartTimer     →
    H.gets _.timerSid >>= maybe
      (timer >>= H.subscribe >>= \sid → H.modify_ _ { timerSid = Just sid })
      (\x→H.unsubscribe x *> H.modify_ _ { timerSid = Nothing })
  ChangeSpeed s  → traverse_ (\y→H.modify_ _ { speed = y }) $ Number.fromString s
  Tick           → H.modify_ $ updateLb ∘ \x→ x {time = 
                      advanceTime (x.speed * 3600.0 * period) x.lastUpdated x.time }
  ChangeSeed s   → traverse_ (\i→H.modify_ _ {seed = i}) $ Int.fromString s
  ChangeColoring c → H.modify_ _ {coloring = c}

-- writing imperative code in functional languages is so fun
strike ∷ Array Mode → Array (Array Cell) → Array (Array Cell)
strike ignored =
  under Compose $ flip St.evalState HSet.empty ∘ traverse case _ of
    Filled cell → do 
      opt ← if any (\x→x ⋄ cell.mode ≡ cell.mode) ignored
            then pure true
            else St.gets (HSet.member cell.mode)
      unless opt (St.modify_ (HSet.insert cell.mode))
      pure ∘ Filled $ Record.merge cell { stricken: opt }
    Unclaimed x → pure (Unclaimed x)
    Empty       → pure Empty
    Header a b  → pure (Header a b)

contextify ∷ State → State
contextify state@{context,modes} = state { modes =
  append context <$> filter (\x→ x ∩ context ≡ ε) modes }

period ∷ Number
period = 1.0/60.0

timer ∷ ∀m. MonadAff m ⇒ m (HS.Emitter Action)
timer = do
  { emitter, listener } ← H.liftEffect HS.create
  _ ← H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds period
    H.liftEffect $ HS.notify listener Tick
  pure emitter

tapper ∷ ∀m. MonadAff m ⇒ Action → m (HS.Emitter Action)
tapper action = do
  { emitter, listener } ← H.liftEffect HS.create
  _ ← H.liftAff $ Aff.forkAff do
    Aff.delay $ Milliseconds 500.0
    H.liftEffect $ HS.notify listener action
  pure emitter
