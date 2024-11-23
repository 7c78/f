module Popper.DOM where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import FFI.DOM.Type (CSSStyleDeclaration, Document, HTMLElement, Window)
import FFI.DOM.Event (Listener)
import Popper.Type (Dimensions, Rect, ScrollState)

foreign import getWindow :: forall node. node -> Window
foreign import getDocument :: forall node. node -> Document

foreign import isNode :: forall any. any -> Boolean
foreign import isElement :: forall any. any -> Boolean
foreign import isHTMLElement :: forall any. any -> Boolean

foreign import getHtml :: Document -> HTMLElement
foreign import getBody :: Document -> HTMLElement
foreign import isHtml :: forall node. node -> Boolean
foreign import isBody :: forall node. node -> Boolean

foreign import _getWindowScroll :: EffectFn1 Window ScrollState
getWindowScroll :: Window -> Effect ScrollState
getWindowScroll = runEffectFn1 _getWindowScroll

foreign import _getElementScroll :: forall element. EffectFn1 element ScrollState
getElementScroll :: forall element. element -> Effect ScrollState
getElementScroll = runEffectFn1 _getElementScroll

foreign import _getViewportRect :: forall element. EffectFn1 element Rect
getViewportRect :: forall element. element -> Effect Rect
getViewportRect = runEffectFn1 _getViewportRect

foreign import _getElementRect :: forall element. EffectFn1 element Rect
getElementRect :: forall element. element -> Effect Rect
getElementRect = runEffectFn1 _getElementRect

foreign import _getElementInnerRect :: forall element. EffectFn1 element Rect
getElementInnerRect :: forall element. element -> Effect Rect
getElementInnerRect = runEffectFn1 _getElementInnerRect

foreign import _getElementDimensions :: forall element. EffectFn1 element Dimensions
getElementDimensions :: forall element. element -> Effect Dimensions
getElementDimensions = runEffectFn1 _getElementDimensions

foreign import _getComputedStyle :: forall element. EffectFn1 element CSSStyleDeclaration
getComputedStyle :: forall element. element -> Effect CSSStyleDeclaration
getComputedStyle = runEffectFn1 _getComputedStyle

foreign import _isOverflowElement :: forall element. EffectFn1 element Boolean
isOverflowElement :: forall element. element -> Effect Boolean
isOverflowElement = runEffectFn1 _isOverflowElement

foreign import _getImmediateOverflowParent :: forall element. EffectFn1 element (Nullable HTMLElement)
getImmediateOverflowParent :: forall element. element -> Effect (Maybe HTMLElement)
getImmediateOverflowParent = map toMaybe <<< runEffectFn1 _getImmediateOverflowParent

foreign import _getOverflowParents :: forall element. EffectFn1 element (Array HTMLElement)
getOverflowParents :: forall element. element -> Effect (Array HTMLElement)
getOverflowParents = runEffectFn1 _getOverflowParents

getOverflowParentRects :: forall element. element -> Effect (Array Rect)
getOverflowParentRects e = do
    parents <- getOverflowParents e
    traverse getElementInnerRect parents

foreign import _getOffsetParent :: forall element. EffectFn1 element (Nullable HTMLElement)
getOffsetParent :: forall element. element -> Effect (Maybe HTMLElement)
getOffsetParent = map toMaybe <<< runEffectFn1 _getOffsetParent

foreign import _isContainingBlock :: forall element. EffectFn1 element Boolean
isContainingBlock :: forall element. element -> Effect Boolean
isContainingBlock = runEffectFn1 _isContainingBlock

foreign import _onScroll :: forall node event. EffectFn2 node (Listener event) Unit
onScroll :: forall node event. node -> Listener event -> Effect Unit
onScroll = runEffectFn2 _onScroll
