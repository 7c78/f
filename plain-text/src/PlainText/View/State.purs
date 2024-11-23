module PlainText.View.State where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Set (Set)
import Data.Tuple.Unsafe (type (&), (&))
import Data.String as String
import Data.List (foldr)
import PlainText.Model.Node (ElementType(..), Node(..), NodeId, maximumNodeId, nodeId)
import PlainText.Model.Path (Path, indexPaths, resolvePath)
import PlainText.Model.Selection (Point, Selection(..), nearestPointBackward, nearestPointForward, rangeForward)
import PlainText.Model.Segment as Segment
import PlainText.Model.Segment (Segment)
import PlainText.Model.Operation (Operation(..), applyOperation)

type EditorState =
    { root      :: Node
    , pathIndex :: Map NodeId Path
    , selection :: Maybe Selection
    }

clearSelection :: EditorState -> EditorState
clearSelection s =
    s{selection = Nothing}

setSelection :: Selection -> EditorState -> EditorState
setSelection sel s =
    s{selection = Just sel}

insertText :: String -> EditorState -> (Set NodeId & EditorState)
insertText t state@{selection, root, pathIndex} =
    case selection of
        Nothing ->
            (Set.empty & state)
        Just sel ->
            let (from & to) = rangeFromTo sel
                root' = applyOperation (Splice from to (Segment.Text t)) root
                selection' = Just $ PointSelection (nextPoint t from)
                pathIndex' = if structureCut from to
                                then indexPaths root'
                                else pathIndex
                state' = state{ root = root'
                              , selection = selection'
                              , pathIndex = pathIndex'
                              }
                dirty = spliceDirty root from to
             in (dirty & state')

deleteBackward :: EditorState -> (Set NodeId & EditorState)
deleteBackward state@{selection, root} =
    case selection of
        Nothing ->
            (Set.empty & state)
        Just (PointSelection y@(_ & yoffset)) ->
            if yoffset > 0 then
                deleteRange (previousPoint y) y state
            else
                case nearestPointBackward root y of
                    Nothing ->
                        (Set.empty & state)
                    Just x ->
                        deleteRange x y state
        Just sel ->
            let (from & to) = rangeFromTo sel
             in deleteRange from to state

deleteForward :: EditorState -> (Set NodeId & EditorState)
deleteForward state@{selection, root} =
    case selection of
        Nothing ->
            (Set.empty & state)
        Just (PointSelection x) ->
            case nearestPointForward root x of
                Nothing ->
                    (Set.empty & state)
                Just y ->
                    deleteRange x y state
        Just sel ->
            let (from & to) = rangeFromTo sel
             in deleteRange from to state

deleteRange :: Point -> Point -> EditorState -> (Set NodeId & EditorState)
deleteRange from to state@{root, pathIndex} =
    let root' = applyOperation (Splice from to Segment.Empty) root
        selection' = Just $ PointSelection from
        pathIndex' = if structureCut from to
                        then indexPaths root'
                        else pathIndex
        state' = state{ root = root'
                      , selection = selection'
                      , pathIndex = pathIndex'
                      }
        dirty = spliceDirty root from to
     in (dirty & state')

structureCut :: Point -> Point -> Boolean
structureCut (xpath & _) (ypath & _) =
    xpath /= ypath

insertLineBreak :: EditorState -> (Set NodeId & EditorState)
insertLineBreak state@{root} =
    let id = maximumNodeId root + 1
        br = LineBreak {id}
     in insertStructure (Segment.Inline br) state

insertParagraph :: EditorState -> (Set NodeId & EditorState)
insertParagraph state@{root} =
    let id = maximumNodeId root + 1
        p = Element {id} (Paragraph {}) []
     in insertStructure (Segment.Block p) state

insertStructure :: Segment -> EditorState -> (Set NodeId & EditorState)
insertStructure segment state@{selection, root} =
    case selection of
        Nothing ->
            (Set.empty & state)
        Just sel ->
            let (from & to) = rangeFromTo sel
                root' = applyOperation (Splice from to segment) root
                selection' = PointSelection <$> nearestPointForward root' from
                pathIndex = indexPaths root'
                state' = state{ root = root'
                              , selection = selection'
                              , pathIndex = pathIndex
                              }
                dirty = spliceDirty root from to
             in (dirty & state')

rangeFromTo :: Selection -> (Point & Point)
rangeFromTo (PointSelection cursor) =
    (cursor & cursor)
rangeFromTo (RangeSelection {anchor, focus}) =
    if rangeForward anchor focus
       then (anchor & focus)
       else (focus & anchor)

spliceDirty :: Node -> Point -> Point -> Set NodeId
spliceDirty root (xpath & _) (ypath & _) =
    Set.unions [ dirty (resolvePath root xpath)
               , dirty (resolvePath root ypath)
               ]
    where
    dirty = foldr step Set.empty
    step {node} =
        Set.insert (nodeId node)

previousPoint :: Point -> Point
previousPoint (path & offset) =
    (path & (offset - 1))

nextPoint :: String -> Point -> Point
nextPoint t (path & offset) =
    (path & (offset + String.length t))
