module FFI.DOM.Document
    ( document
    , createElement
    , createTextNode
    , getElementById
    ) where

import Effect (Effect)
import FFI.DOM.Type (Document)

foreign import document :: Document

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement
foreign import createElement :: forall element. String -> Effect element

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document/createTextNode
foreign import createTextNode :: forall text. String -> Effect text

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document/getElementById
foreign import getElementById :: forall element. String -> Effect element
