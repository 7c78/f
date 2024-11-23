module PlainText.View.Selection where

import Prelude
import Data.Map (Map)
import Data.Map.Unsafe as Map
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Array.Unsafe as Array
import Data.List (foldl)
import Data.Tuple.Unsafe ((&))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import PlainText.Model.Node (NodeId, isText)
import PlainText.Model.Path (Path)
import PlainText.Model.Selection (Point, Selection(..))
import PlainText.View.DOM (DOMNode, DOMPoint, DOMSelection, DOMRange)
import PlainText.View.Node (ViewNode(..), closest, domText, emptyText, viewNodeId)

foreign import _normalizeDOMPoint :: EffectFn1 DOMPoint (Nullable DOMPoint)
normalizeDOMPoint :: DOMPoint -> Effect (Maybe DOMPoint)
normalizeDOMPoint = map toMaybe <<< runEffectFn1 _normalizeDOMPoint

foreign import _selectionWithin :: EffectFn2 DOMSelection DOMNode Boolean
selectionWithin :: DOMSelection -> DOMNode -> Effect Boolean
selectionWithin = runEffectFn2 _selectionWithin

foreign import getSelection :: Effect DOMSelection
foreign import getAnchor :: DOMSelection -> DOMPoint
foreign import getFocus :: DOMSelection -> DOMPoint
foreign import isCollapsed :: DOMSelection -> Boolean

foreign import _renderDOMSelection :: EffectFn3 DOMSelection DOMPoint DOMPoint Unit
renderDOMSelection :: DOMSelection -> DOMPoint -> DOMPoint -> Effect Unit
renderDOMSelection = runEffectFn3 _renderDOMSelection

foreign import _clearDOMSelection :: EffectFn1 DOMSelection Unit
clearDOMSelection :: DOMSelection -> Effect Unit
clearDOMSelection = runEffectFn1 _clearDOMSelection

toPointEmptyText :: Map NodeId Path -> DOMPoint -> Effect Point
toPointEmptyText pathIndex point = do
    text <- emptyText point
    let path = Map.lookup (viewNodeId text) pathIndex
    pure $ path & 0

toPoint :: DOMNode -> Map NodeId Path -> DOMPoint -> Effect Point
toPoint rootDOM pathIndex {node, offset} = do
    text <- closest rootDOM node isText
    let path = Map.lookup (viewNodeId text) pathIndex
    pure (path & offset)

index :: ViewNode -> Path -> ViewNode
index = foldl step where
    step (ViewNode {children}) i =
        children `Array.index` i

fromPoint :: Point -> ViewNode -> DOMPoint
fromPoint (path & offset) root =
    let vnode = root `index` path
     in {node: domText vnode, offset}

fromSelection :: Selection -> ViewNode -> DOMRange
fromSelection (PointSelection point) root =
    let domPoint = fromPoint point root
     in { anchor: domPoint
        , focus: domPoint
        }
fromSelection (RangeSelection {anchor, focus}) root =
    { anchor: fromPoint anchor root
    , focus:  fromPoint focus root
    }
