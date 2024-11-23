module Popper.Type where

import Prelude

data PlacementBase
    = Top
    | Right
    | Bottom
    | Left

data PlacementAlignment
    = Center
    | Start
    | End

data Placement = Placement PlacementBase PlacementAlignment

getOppositePlacement :: Placement -> Placement
getOppositePlacement (Placement base align) =
    Placement base' align
    where base' = case base of
            Top    -> Bottom
            Bottom -> Top
            Left   -> Right
            Right  -> Left

type Dimensions =
    { width  :: Number
    , height :: Number
    }

type Coords =
    { left :: Number
    , top  :: Number
    }

type Rect =
    { width  :: Number
    , height :: Number
    , left   :: Number
    , top    :: Number
    , right  :: Number
    , bottom :: Number
    }

toCoords :: Rect -> Coords
toCoords {left, top} = {left, top}

toRect :: Dimensions -> Coords -> Rect
toRect {width, height} {left, top} =
    { width
    , height
    , left
    , top
    , bottom: top + height
    , right: left + width
    }

type OverflowState =
    { overflowTop    :: Number
    , overflowRight  :: Number
    , overflowBottom :: Number
    , overflowLeft   :: Number
    }

type ScrollState =
    { scrollLeft :: Number
    , scrollTop  :: Number
    }
