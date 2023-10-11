module Main.PointerEvents (onPointerDown,onPointerUp,onPointerMove,onPointerEnter,onPointerLeave,mouseHandler,module Web.PointerEvent) where

import Halogen.HTML (IProp)
import Halogen.HTML.Events (handler)
import Main.Common ((∘))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.PointerEvent (PointerEvent)
import Web.PointerEvent.EventTypes as ET

onPointerDown ∷ ∀r i. (PointerEvent → i) → IProp (onPointerDown ∷ PointerEvent | r) i
onPointerDown = handler ET.pointerdown ∘ mouseHandler

onPointerUp ∷ ∀r i. (PointerEvent → i) → IProp (onPointerUp ∷ PointerEvent | r) i
onPointerUp = handler ET.pointerup ∘ mouseHandler

onPointerMove ∷ ∀r i. (PointerEvent → i) → IProp (onPointerMove ∷ PointerEvent | r) i
onPointerMove = handler ET.pointermove ∘ mouseHandler

onPointerEnter ∷ ∀r i. (PointerEvent → i) → IProp (onPointerEnter ∷ PointerEvent | r) i
onPointerEnter = handler ET.pointerenter ∘ mouseHandler

onPointerLeave ∷ ∀r i. (PointerEvent → i) → IProp (onPointerLeave ∷ PointerEvent | r) i
onPointerLeave = handler ET.pointerleave ∘ mouseHandler

mouseHandler ∷ forall i. (PointerEvent → i) → Event → i
mouseHandler = unsafeCoerce