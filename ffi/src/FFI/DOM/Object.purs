module FFI.DOM.Object
    ( module X
    , insertProperties, (.+)
    , deleteProperty, (.-)
    ) where

import FFI.Object ((!)) as X
import FFI.AssocArray ((:=)) as X

import Prelude
import Effect (Effect)
import FFI.AssocArray (AssocArray)

infix 4 insertProperties as .+
foreign import insertProperties :: forall a o. o -> AssocArray a -> Effect Unit

infix 4 deleteProperty as .-
foreign import deleteProperty :: forall o. o -> String -> Effect Unit
