module FFI.DOM.Type
    ( Window
    , Document
    , EventTarget
    , Node
    , Element
    , HTMLElement
    , CSSStyleDeclaration
    ) where

-- | https://developer.mozilla.org/en-US/docs/Web/API/Window
foreign import data Window :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/Document
foreign import data Document :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/EventTarget
foreign import data EventTarget :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element
foreign import data Node :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/Element
foreign import data Element :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement
foreign import data HTMLElement :: Type

-- | https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration
foreign import data CSSStyleDeclaration :: Type
