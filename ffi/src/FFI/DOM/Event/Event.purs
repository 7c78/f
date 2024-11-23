module FFI.DOM.Event.Event
    ( EventPhase(..), eventPhase
    , cancelable, defaultPrevented, target
    , stopPropagation, stopImmediatePropagation, preventDefault
    ) where

import Prelude
import Effect (Effect)
import FFI.Object ((!))

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/target
target :: forall @target event. event -> target
target e = e ! "target"

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/eventPhase
data EventPhase
    = None
    | CapturingPhase
    | AtTarget
    | BubblingPhase

-- https://developer.mozilla.org/en-US/docs/Web/API/Event/eventPhase
eventPhase :: forall event. event -> EventPhase
eventPhase e = phase $ e ! "eventPhase"
    where phase 1 = CapturingPhase
          phase 2 = AtTarget
          phase 3 = BubblingPhase
          phase _ = None

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/cancelable
cancelable :: forall event. event -> Boolean
cancelable e = e ! "cancelable"

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/defaultPrevented
defaultPrevented :: forall event. event -> Boolean
defaultPrevented e = e ! "defaultPrevented"

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation
foreign import stopPropagation :: forall event. event -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/stopImmediatePropagation
foreign import stopImmediatePropagation :: forall event. event -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault
foreign import preventDefault :: forall event. event -> Effect Unit
