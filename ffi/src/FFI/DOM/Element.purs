module FFI.DOM.Element
    ( append
    , after
    , before
    , replaceWith
    , remove
    ) where

import Prelude
import Effect (Effect)

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element/append
foreign import append :: forall element. element -> element -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element/after
foreign import after :: forall element. element -> element -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element/before
foreign import before :: forall element. element -> element -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element/replaceWith
foreign import replaceWith :: forall element. element -> element -> Effect Unit

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element/remove
foreign import remove :: forall element. element -> Effect Unit
