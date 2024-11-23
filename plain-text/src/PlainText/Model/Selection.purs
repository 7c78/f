module PlainText.Model.Selection where

import Prelude
import Data.Tuple.Unsafe (type (&), (&))
import Data.Maybe (Maybe(..))
import Data.List (List(..), foldr, (:))
import Data.List.Unsafe (unsnoc)
import Data.Array (length) as Array
import Data.Array.Unsafe (index) as Array
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.String as String
import PlainText.Model.Path (Path, resolvePath)
import PlainText.Model.Node (Node(..), children, lengthText)

type TextOffset = Int
type Point = (Path & TextOffset)

type Range =
    { anchor :: Point
    , focus  :: Point
    }

data Selection
    = PointSelection Point
    | RangeSelection Range

derive instance Eq Selection

rangeBackward :: Point -> Point -> Boolean
rangeBackward anchor focus = anchor > focus

rangeForward :: Point -> Point -> Boolean
rangeForward anchor focus = anchor < focus

endPoint :: Node -> Maybe Point
endPoint (Text _ _ t) =
    Just (Nil & String.length t)
endPoint (LineBreak _) =
    Nothing
endPoint node =
    foldrWithIndex step Nothing (children node)
    where
    step i child Nothing =
        case endPoint child of
            Nothing ->
                Nothing
            Just (path & offset) ->
                Just ((i : path) & offset)
    step _ _ z =
        z

endPoint' :: Array Node -> Int -> Maybe Point
endPoint' _ i | i < 0 =
    Nothing
endPoint' nodes i =
    case endPoint (nodes `Array.index` i) of
        Nothing ->
            endPoint' nodes (i - 1)
        Just (path & offset) ->
            Just ((i : path) & offset)

nearestPointBackward :: Node -> Point -> Maybe Point
nearestPointBackward _ (path & offset) | offset > 0 =
    Just (path & (offset - 1))
nearestPointBackward root (path & _) =
    let (path' & childIndex) = unsnoc path
        path'' = resolvePath root path'
        z = {childIndex, mPoint: Nothing}
     in _.mPoint $ foldr step z path''
    where
    step {node, index} {childIndex, mPoint} =
        case mPoint of
            Nothing ->
                let mPoint = endPoint' (children node) (childIndex - 1)
                 in {mPoint, childIndex: index}
            Just (path & offset) ->
                let mPoint = Just ((childIndex : path) & offset)
                 in {childIndex: index, mPoint}

startPoint :: Node -> Maybe Point
startPoint (Text _ _ _) =
    Just (Nil & 0)
startPoint (LineBreak _) =
    Nothing
startPoint node =
    foldlWithIndex step Nothing (children node)
    where
    step i Nothing child =
        case startPoint child of
            Nothing ->
                Nothing
            Just (path & offset) ->
                Just ((i : path) & offset)
    step _ z _ =
        z

startPoint' :: Array Node -> Int -> Maybe Point
startPoint' nodes i | i >= Array.length nodes =
    Nothing
startPoint' nodes i =
    case startPoint (nodes `Array.index` i) of
        Nothing ->
            startPoint' nodes (i + 1)
        Just (path & offset) ->
            Just ((i : path) & offset)

nearestPointForward :: Node -> Point -> Maybe Point
nearestPointForward root (path & offset) =
    let path' = resolvePath root path
        (path'' & last) = unsnoc path'
        z = {childIndex: last.index, mPoint: Nothing}
     in if offset < lengthText last.node then
            Just (path & (offset + 1))
        else
            _.mPoint $ foldr step z path''
            where
            step {node, index} {childIndex, mPoint} =
                case mPoint of
                    Nothing ->
                        let mPoint = startPoint' (children node) (childIndex + 1)
                         in {mPoint, childIndex: index}
                    Just (path & offset) ->
                        let mPoint = Just ((childIndex : path) & offset)
                         in {childIndex: index, mPoint}
