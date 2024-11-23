module Popper.App.Main where

import Prelude hiding (flip)
import Data.Array as Array
import Data.Traversable (for_)
import Effect (Effect)
import FFI.DOM.Document (getElementById)
import FFI.DOM.Event (addEventListener, listener)
import Popper.State (computeCoords, eval, flip, floatCSS, mkFloat, offset, translateCSS)
import Popper.Type (Placement(..), PlacementAlignment(..), PlacementBase(..))
import Popper.DOM (getOverflowParentRects, getOverflowParents, getViewportRect, getWindow, onScroll)

case1 :: Effect Unit
case1 = do
    ref <- getElementById "case1_ref"
    float <- getElementById "case1_float"
    floatCSS float

    let place placement = do
            state <- mkFloat ref float
            let s = state {placement = placement}
                coords = eval computeCoords s
            translateCSS float coords

        click id placement = do
            btn <- getElementById id
            addEventListener btn "click" $ listener \_ ->
                place placement

    place (Placement Top Center)

    click "case1_btn_ts" (Placement Top    Start)
    click "case1_btn_t"  (Placement Top    Center)
    click "case1_btn_te" (Placement Top    End)
    click "case1_btn_rs" (Placement Right  Start)
    click "case1_btn_r"  (Placement Right  Center)
    click "case1_btn_re" (Placement Right  End)
    click "case1_btn_bs" (Placement Bottom Start)
    click "case1_btn_b"  (Placement Bottom Center)
    click "case1_btn_be" (Placement Bottom End)
    click "case1_btn_ls" (Placement Left   Start)
    click "case1_btn_l"  (Placement Left   Center)
    click "case1_btn_le" (Placement Left   End)

case2 :: Effect Unit
case2 = do
    ref <- getElementById "case2_ref"
    float <- getElementById "case2_float"
    floatCSS float

    let place placement = do
            state <- mkFloat ref float
            let s = state {placement = placement}
                coords = eval (offset 10.0 <<< computeCoords) s
            translateCSS float coords

        click id placement = do
            btn <- getElementById id
            addEventListener btn "click" $ listener \_ ->
                place placement

    place (Placement Top Center)
    click "case2_btn_t" (Placement Top    Center)
    click "case2_btn_r" (Placement Right  Center)
    click "case2_btn_b" (Placement Bottom Center)
    click "case2_btn_l" (Placement Left   Center)

case3 :: Effect Unit
case3 = do
    ref <- getElementById "case3_ref"
    float <- getElementById "case3_float"
    floatCSS float

    floatOverflowParents <- getOverflowParents float
    let floatWin = getWindow float

    let place = do
            state <- mkFloat ref float
            parentRects <- getOverflowParentRects float
            viewportRect <- getViewportRect float
            let overflowRects = Array.snoc parentRects viewportRect
            let s = state { placement = Placement Top Center
                          , overflowingRects = overflowRects }
                coords = eval (flip computeCoords <<< computeCoords) s
            translateCSS float coords

    let scrollListener = listener \_ -> place

    for_ floatOverflowParents \e ->
        onScroll e scrollListener
    onScroll floatWin scrollListener

    place

main :: Effect Unit
main = do
    case1
    case2
    case3
