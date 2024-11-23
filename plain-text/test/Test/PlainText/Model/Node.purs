module Test.PlainText.Model.Node where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Effect.Exception.Unsafe (unsafeThrow)
import PlainText.Model.Node (ElementType(..), HeadingAttributes, LinkAttributes, Node(..), NodeMetadata, TextAttributes, nodeId)

root :: NodeMetadata -> Array Node -> Node
root = Root

h1 :: NodeMetadata -> Array Node -> Node
h1 m = h' m {level: 1}

h2 :: NodeMetadata -> Array Node -> Node
h2 m = h' m {level: 2}

h' :: NodeMetadata -> HeadingAttributes -> Array Node -> Node
h' m = Element m <<< Heading

p :: NodeMetadata -> Array Node -> Node
p m = Element m (Paragraph {})

link' :: NodeMetadata -> LinkAttributes -> Array Node -> Node
link' m = Element m <<< Link

link :: NodeMetadata -> Array Node -> Node
link m = link' m {href: ""}

br :: NodeMetadata -> Node
br = LineBreak

text' :: NodeMetadata -> TextAttributes -> String -> Node
text' = Text

text :: NodeMetadata -> String -> Node
text m = text' m {italic: false, bold: false}

italic :: TextAttributes
italic = {italic: true, bold: false}

bold :: TextAttributes
bold = {italic: false, bold: true}

id :: Array Node -> Int -> Node
id ns i =
    case Array.find (\n -> nodeId n == i) ns of
        Nothing -> unsafeThrow "invalid id"
        Just n  -> n
