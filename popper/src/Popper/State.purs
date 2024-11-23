module Popper.State where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FFI.DOM.Object ((!), (.+), (:=))
import Popper.Type (Coords, Dimensions, Placement(..), PlacementAlignment(..), PlacementBase(..), Rect, ScrollState, OverflowState, toCoords, toRect, getOppositePlacement)
import Popper.DOM (getDocument, getElementDimensions, getElementRect, getElementScroll, getHtml, getOffsetParent, isOverflowElement)
import Popper.Position as P

type FloatingState =
    { reference ::
        { rect :: Rect
        }
    , float ::
        { dimensions :: Dimensions
        }
    , containingBlock ::
        { rect :: Rect
        , scroll :: ScrollState
        }
    , placement :: Placement
    , overflowingRects :: Array Rect
    }

data FloatingUpdate = FloatingUpdate Coords FloatingState

translateCSS :: forall element. element -> Coords -> Effect Unit
translateCSS e {left, top} = do
    let css = "translate(" <> show left <> "px, " <> show top <> "px)"
    e!"style" .+ "transform" := css

floatCSS :: forall element. element -> Effect Unit
floatCSS e =
    e!"style" .+ "position" := "absolute"
              <> "top" := "0"
              <> "left" := "0"

mkFloat :: forall element. element -> element -> Effect FloatingState
mkFloat ref float = do
    floatSize <- getElementDimensions float

    containingBlock <- do
        mParent <- getOffsetParent float
        case mParent of
            Nothing -> do
                let html = getHtml $ getDocument float
                    -- ^ the *initial* containing block
                rect <- getElementRect html
                scroll <- ifM (isOverflowElement html)
                              (getElementScroll html)
                              (pure {scrollLeft: 0.0, scrollTop: 0.0})
                pure {rect, scroll}
            Just parent -> do
                rect <- getElementRect parent
                scroll <- getElementScroll parent
                pure {rect, scroll}

    refRect <- translateRectToContainingBlock containingBlock
              <$> getElementRect ref

    pure { reference:
            { rect: refRect
            }
         , float:
            { dimensions: floatSize
            }
         , containingBlock
         , placement: Placement Top Center
         , overflowingRects: []
         }

eval :: (FloatingState -> FloatingUpdate) -> FloatingState -> Coords
eval compute s0 =
    let (FloatingUpdate coords _) = compute s0
     in coords

computeCoords :: FloatingState -> FloatingUpdate
computeCoords s@{reference, float, placement} =
    let coords = P.computeCoords reference.rect float.dimensions placement
     in FloatingUpdate coords s

offset :: Number -> FloatingUpdate -> FloatingUpdate
offset n (FloatingUpdate coords s@{placement}) =
    let coords' = P.offset n placement coords
     in FloatingUpdate coords' s

flip :: (FloatingState -> FloatingUpdate) -> FloatingUpdate -> FloatingUpdate
flip continue (FloatingUpdate coords s@{float, containingBlock, overflowingRects, placement}) =
    let opPlacement = getOppositePlacement placement
        FloatingUpdate opCoords opS = continue (s {placement = opPlacement})
        opRect = translateRectToViewport containingBlock
               $ toRect float.dimensions opCoords
        opOverflow = isOverflowingAtPlacement opPlacement
                   $ P.computeOverflow overflowingRects opRect

        rect = translateRectToViewport containingBlock
             $ toRect float.dimensions coords
        overflow = isOverflowingAtPlacement placement
                 $ P.computeOverflow overflowingRects rect

     in if not overflow || opOverflow
           then FloatingUpdate coords s
           else FloatingUpdate opCoords opS

translateRectToContainingBlock
    :: {rect :: Rect, scroll :: ScrollState}
    -> Rect
    -> Rect
translateRectToContainingBlock {rect, scroll} =
        P.translateRectByScrollOffset scroll
    <<< P.translateRectToCoords (toCoords rect)

translateRectToViewport
    :: {rect :: Rect, scroll :: ScrollState}
    -> Rect
    -> Rect
translateRectToViewport {rect, scroll} =
    let coords = toCoords rect
        negCoords = { left: -coords.left
                    , top:  -coords.top
                    }
        negScroll = { scrollLeft: -scroll.scrollLeft
                    , scrollTop:  -scroll.scrollTop
                    }
     in     P.translateRectByScrollOffset negScroll
        <<< P.translateRectToCoords negCoords

isOverflowingAtPlacement :: Placement -> OverflowState -> Boolean
isOverflowingAtPlacement placement overflow =
    case placement of
        Placement Top    _ -> overflow.overflowTop > 0.0
        Placement Bottom _ -> overflow.overflowBottom > 0.0
        Placement Left   _ -> overflow.overflowLeft > 0.0
        Placement Right  _ -> overflow.overflowRight > 0.0
