module PlainText.View.DOM where

import Prelude

-- | https://developer.mozilla.org/en-US/docs/Web/API/Node
foreign import data DOMNode :: Type

foreign import eqDOMNode :: DOMNode -> DOMNode -> Boolean
instance Eq DOMNode where eq = eqDOMNode

-- | https://developer.mozilla.org/en-US/docs/Web/API/Selection
foreign import data DOMSelection :: Type

type DOMPoint =
    { node   :: DOMNode
    , offset :: Int
    }

type DOMRange =
    { anchor :: DOMPoint
    , focus  :: DOMPoint
    }

foreign import isText :: DOMNode -> Boolean
foreign import isElement :: DOMNode -> Boolean
