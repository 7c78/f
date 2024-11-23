module Popper.Position where

import Prelude
import Data.Foldable.Unsafe (foldr1)
import Popper.Type (Coords, Dimensions, OverflowState, Placement(..), PlacementAlignment(..), PlacementBase(..), Rect, ScrollState)

-- | Move a Rect's coordinates from its current origin to a new coordinate
-- system.
translateRectToCoords :: Coords -> Rect -> Rect
translateRectToCoords {left, top} rect =
    { width:  rect.width
    , height: rect.height
    , top:    rect.top - top
    , bottom: rect.bottom - top
    , left:   rect.left - left
    , right:  rect.right - left
    }

-- | Adjust a Rect based on scroll offsets.
translateRectByScrollOffset :: ScrollState -> Rect -> Rect
translateRectByScrollOffset {scrollLeft, scrollTop} rect =
    { width:  rect.width
    , height: rect.height
    , top:    rect.top + scrollTop
    , bottom: rect.bottom + scrollTop
    , left:   rect.left + scrollLeft
    , right:  rect.right + scrollLeft
    }

-- | Compute the common region (intersection) of a set of Rects.
computeIntersectingRect :: Array Rect -> Rect
computeIntersectingRect rects =
    foldr1 (\x z -> let top = max x.top z.top
                        right = min x.right z.right
                        bottom = min x.bottom z.bottom
                        left = max x.left z.left
                     in { top
                        , right
                        , bottom
                        , left
                        , width: right - left
                        , height: bottom - top
                        })
           rects

-- | Compute the overflow of an element relative to a set of overflowing
-- boundaries.
--
--   * Positive values: the number of pixels the element overflows beyond the
--   intersecting boundary.
--   * Negative values: the number of pixels remaining before the element will
--   overflow.
--   * 0: the element exactly aligns with the boundary.
computeOverflow
    :: Array Rect
    -> Rect
    -> OverflowState
computeOverflow overflowingRects rect =
    let overflowingRect = computeIntersectingRect overflowingRects
     in { overflowTop:    overflowingRect.top - rect.top
        , overflowBottom: rect.bottom - overflowingRect.bottom
        , overflowLeft:   overflowingRect.left - rect.left
        , overflowRight:  rect.right - overflowingRect.right
        }

computeCoords :: Rect -> Dimensions -> Placement -> Coords
computeCoords ref float (Placement base align) = {left, top} where
    left = case base, align of
        Left,  _      -> ref.left - float.width
        Right, _      -> ref.right
        _,     Start  -> ref.left
        _,     End    -> ref.right - float.width
        _,     Center -> ref.left + ref.width / 2.0 - float.width / 2.0
    top = case base, align of
        Top,    _      -> ref.top - float.height
        Bottom, _      -> ref.bottom
        _,      Start  -> ref.top
        _,      End    -> ref.bottom - float.height
        _,      Center -> ref.top + ref.height / 2.0 - float.height / 2.0

offset :: Number -> Placement -> Coords -> Coords
offset n (Placement base _) {top, left} =
    {left: left', top: top'}
  where
    left' = case base of
        Left  -> left - n
        Right -> left + n
        _     -> left
    top' = case base of
        Top    -> top - n
        Bottom -> top + n
        _      -> top

alignmentOffset :: Number -> Placement -> Coords -> Coords
alignmentOffset n (Placement base align) {top, left} =
    {left: left', top: top'}
  where
    left' = case base, align of
        Top,    Start -> left + n
        Bottom, Start -> left + n
        Top,    End   -> left - n
        Bottom, End   -> left - n
        _,      _     -> left
    top' = case base, align of
        Left,  Start -> top + n
        Right, Start -> top + n
        Left,  End   -> top - n
        Right, End   -> top - n
        _,     _     -> top
