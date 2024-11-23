module PlainText.Model.Node where

import Prelude
import Data.Array.Unsafe ((!), deleteAt, insertAt, updateAt)
import Data.Array (foldl, splitAt) as Array
import Data.String (splitAt, length) as String
import Effect.Exception.Unsafe (unsafeThrow)

type NodeId = Int
type NodeMetadata =
    { id :: NodeId
    }

type HeadingAttributes =
    { level :: Int
    }

type ParagraphAttributes =
    {
    }

type LinkAttributes =
    { href :: String
    }

type TextAttributes =
    { bold   :: Boolean
    , italic :: Boolean
    }

data Node
    = Root NodeMetadata (Array Node)
    | Element NodeMetadata ElementType (Array Node)
    | Text NodeMetadata TextAttributes String
    | LineBreak NodeMetadata

data ElementType
    = Heading HeadingAttributes
    | Paragraph ParagraphAttributes
    | Link LinkAttributes

derive instance Eq Node
derive instance Eq ElementType

instance Show Node where
    show (Root _ children) =
        "Root" <> show children
    show (Text {id} attrs content) =
        "(" <> show id <> ") Text " <> show content <> " " <> show attrs
    show (LineBreak {id}) =
        "(" <> show id <> ") LineBreak"
    show (Element {id} etype children) =
        "(" <> show id <> ") " <> show etype <> " " <> show children

instance Show ElementType where
    show (Heading attrs) =
        "Heading " <> show attrs
    show (Paragraph attrs) =
        "Paragraph " <> show attrs
    show (Link attrs) =
        "Link " <> show attrs

isRoot :: Node -> Boolean
isRoot (Root _ _) = true
isRoot _          = false

isText :: Node -> Boolean
isText (Text _ _ _) = true
isText _            = false

updateNodeId :: NodeId -> Node -> Node
updateNodeId id (Root _ s)       = Root {id} s
updateNodeId id (Element _ t s)  = Element {id} t s
updateNodeId id (Text _ attrs s) = Text {id} attrs s
updateNodeId id (LineBreak _)    = LineBreak {id}

nodeId :: Node -> NodeId
nodeId (Root {id} _)      = id
nodeId (Element {id} _ _) = id
nodeId (Text {id} _ _)    = id
nodeId (LineBreak {id})   = id

childAt :: Node -> Int -> Node
childAt node i = children node ! i

children :: Node -> Array Node
children (Root _ xs)      = xs
children (Element _ _ xs) = xs
children _                = []

updateChildren :: Array Node -> Node -> Node
updateChildren xs (Root m _) =
    Root m xs
updateChildren xs (Element m t _) =
    Element m t xs
updateChildren _ _ =
    unsafeThrow "updateChildren: invalid call"

updateChildAt :: Int -> Node -> Node -> Node
updateChildAt i child node =
    let children' = updateAt i child (children node)
     in updateChildren children' node

insertChildAt :: Int -> Node -> Node -> Node
insertChildAt i child node =
    let children' = insertAt i child (children node)
     in updateChildren children' node

deleteChildAt :: Int -> Node -> Node
deleteChildAt i node =
    let children' = deleteAt i (children node)
     in updateChildren children' node

childrenFrom :: Node -> Int -> Array Node
childrenFrom node f =
    let {after} = Array.splitAt f (children node)
     in after

childrenUntil :: Node -> Int -> Array Node
childrenUntil node t =
    let {before} = Array.splitAt t (children node)
     in before

childrenFromUntil :: Node -> Int -> Int -> Array Node
childrenFromUntil node f t =
    let {after} = Array.splitAt f (children node)
        {before} = Array.splitAt (t - f) after
     in before

text :: Node -> String
text (Text _ _ t) = t
text _            = ""

lengthText :: Node -> Int
lengthText = String.length <<< text

updateText :: String -> Node -> Node
updateText t (Text m attrs _) =
    Text m attrs t
updateText _ _ =
    unsafeThrow "updateText: invalid call"

textFrom :: Node -> Int -> String
textFrom node f =
    let {after} = String.splitAt f (text node)
     in after

textUntil :: Node -> Int -> String
textUntil node t =
    let {before} = String.splitAt t (text node)
     in before

textFromUntil :: Node -> Int -> Int -> String
textFromUntil node f t =
    let {after} = String.splitAt f (text node)
        {before} = String.splitAt (t - f) after
     in before

foldl :: forall b. (b -> Node -> b) -> b -> Node -> b
foldl f z node = step z node where
    step z node@(Text _ _ _) =
        f z node
    step z node@(LineBreak _ ) =
        f z node
    step z node =
        Array.foldl step (f z node) (children node)

maximumNodeId :: Node -> Int
maximumNodeId = foldl f 0 where
    f z node =
        max z (nodeId node)
