module FFI.DOM.Event.KeyboardEvent
    ( KeyboardEvent
    , key
    ) where

import FFI.Object ((!))

-- | https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent
foreign import data KeyboardEvent :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key
key :: KeyboardEvent -> String
key e = e ! "key"
