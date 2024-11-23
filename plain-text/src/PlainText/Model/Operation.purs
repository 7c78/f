module PlainText.Model.Operation where

import PlainText.Model.Node (Node)
import PlainText.Model.Selection (Point)
import PlainText.Model.Path (Path)
import PlainText.Model.Segment as Segment
import PlainText.Model.Segment (Segment)
import PlainText.Model.Splice.Internal (splice, spliceText, spliceInline, spliceBlock)
import PlainText.Model.NodeOperation.Internal (deleteNode, insertNode, joinTexts)

data Operation
    = Splice Point Point Segment
    | InsertNode Path Node
    | DeleteNode Path
    | JoinTexts Path Int Int

applyOperation :: Operation -> Node -> Node
applyOperation (Splice x y segment) =
    case segment of
        Segment.Empty ->
            splice x y
        Segment.Text ins ->
            spliceText x y ins
        Segment.Inline ins ->
            spliceInline x y ins
        Segment.Block ins ->
            spliceBlock x y ins
applyOperation (InsertNode path node) =
    insertNode path node
applyOperation (DeleteNode path) =
    deleteNode path
applyOperation (JoinTexts path from to) =
    joinTexts path from to
