module PlainText.Model.Segment where

import PlainText.Model.Node (Node)

data Segment
    = Empty
    | Text String
    | Inline Node
    | Block Node
