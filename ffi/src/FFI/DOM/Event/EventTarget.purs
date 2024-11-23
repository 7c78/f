module FFI.DOM.Event.EventTarget
    ( Listener, listener
    , addEventListener
    , removeEventListener
    ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

newtype Listener event = Listener (EffectFn1 event Unit)

-- | Make a JavaScript function from a PureScript `Effect` function.
listener :: forall @event. (event -> Effect Unit) -> Listener event
listener = Listener <<< mkEffectFn1

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener
foreign import addEventListener
    :: forall event target.
       target
    -> String -- ^ event type name
    -> Listener event
    -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/removeEventListener
foreign import removeEventListener
    :: forall event target.
       target
    -> String -- ^ event type name
    -> Listener event
    -> Effect Unit
