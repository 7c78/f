module Test.PlainText.Model.NodeWithSelection where

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..), Replacement(..))
import Data.Array (snoc, concatMap, length) as Array
import Data.Array.Unsafe (index) as Array
import Data.List (List(..))
import Data.List.Unsafe (arrayList)
import Data.Tuple (fst, snd)
import Data.Tuple.Unsafe (type (&), (&))
import Data.Map.Unsafe as Map
import PlainText.Model.Node (ElementType(..), HeadingAttributes, LinkAttributes, Node(..), NodeMetadata, TextAttributes)
import PlainText.Model.Selection (Point)
import PlainText.Model.Path (indexPaths)

newtype Point' = Point'
    { textId :: Int
    , offset :: Int
    }

type Node' = Node & Array Point'

selection :: Node' -> Point & Point
selection (node & points) =
    let pathIndex = indexPaths node
        p0 = if Array.length points > 0 then
                let Point' {textId: textId0, offset: offset0} = points `Array.index` 0
                    path0 = Map.lookup textId0 pathIndex
                 in path0 & offset0
             else
                Nil & 0
        p1 = if Array.length points > 1 then
                let Point' {textId: textId1, offset: offset1} = points `Array.index` 1
                    path1 = Map.lookup textId1 pathIndex
                 in path1 & offset1
             else
                p0
     in p0 & p1

node :: Node' -> Node
node (n & _) = n

fstSnd :: Array Node' -> Array Node & Array Point'
fstSnd nodes' =
    let nodes = map fst nodes'
        points = Array.concatMap snd nodes'
     in nodes & points

root :: NodeMetadata -> Array Node' -> Node'
root m children' =
    let children & points = fstSnd children'
        n = Root m children
     in n & points

h' :: NodeMetadata -> HeadingAttributes -> Array Node' -> Node'
h' m attrs children' =
    let children & points = fstSnd children'
        n = Element m (Heading attrs) children
     in n & points

h1 :: NodeMetadata -> Array Node' -> Node'
h1 m = h' m {level: 1}

h2 :: NodeMetadata -> Array Node' -> Node'
h2 m = h' m {level: 2}

p :: NodeMetadata -> Array Node' -> Node'
p m children' =
    let children & points = fstSnd children'
        n = Element m (Paragraph {}) children
     in n & points

link' :: NodeMetadata -> LinkAttributes -> Array Node' -> Node'
link' m attrs children' =
    let children & points = fstSnd children'
        n = Element m (Link attrs) children
     in n & points

link :: NodeMetadata -> Array Node' -> Node'
link m = link' m {href: ""}

br :: NodeMetadata -> Node'
br m = LineBreak m & []

text' :: NodeMetadata -> TextAttributes -> String -> Node'
text' m@{id} attrs content =
    let offsets = findOffsets "|" content
        points = map (\offset -> Point' {textId: id, offset}) offsets
        content' = String.replaceAll (Pattern "|") (Replacement "") content
        n = Text m attrs content'
     in n & points

text :: NodeMetadata -> String -> Node'
text m = text' m {italic: false, bold: false}

italic :: TextAttributes
italic = {italic: true, bold: false}

bold :: TextAttributes
bold = {italic: false, bold: true}

findOffsets :: String -> String -> Array Int
findOffsets pattern =
    loop [] 0
    where
    loop z i s =
        case String.uncons s of
            Nothing           -> z
            Just {head, tail} ->
                if String.singleton head == pattern
                   then loop (z `Array.snoc` i) i tail
                   else loop z (i + 1) tail

spec :: Spec Unit
spec = do
    describe "Test.PlainText.Model.NodeWithSelection" do
        describe "findOffsets" do
            it "no points" do
                findOffsets "|" "hello world" `shouldEqual`
                    []

            it "point at start" do
                findOffsets "|" "|hello world" `shouldEqual`
                    [0]

            it "cursor at start" do
                findOffsets "|" "||hello world" `shouldEqual`
                    [0, 0]

            it "point at middle" do
                findOffsets "|" "hell|o world" `shouldEqual`
                    [4]

            it "cursor at middle" do
                findOffsets "|" "hell||o world" `shouldEqual`
                    [4, 4]

            it "range at middle" do
                findOffsets "|" "hell|o| world" `shouldEqual`
                    [4, 5]

            it "point at end" do
                findOffsets "|" "hello world|" `shouldEqual`
                    [11]

        describe "node" do
            it "construct node with points" do
                let r = root {id: 0}
                            [ h1 {id: 1}
                                [ text {id: 2} "hello"
                                ]
                            , p {id: 3}
                                [ text {id: 4} "w|o|rld"
                                ]
                            ]
                    start & end = selection r

                start `shouldEqual` (arrayList [1, 0] & 1)
                end `shouldEqual` (arrayList [1, 0] & 2)
                node r `shouldEqual`
                    (node $ root {id: 0}
                                [ h1 {id: 1}
                                    [ text {id: 2} "hello"
                                    ]
                                , p {id: 3}
                                    [ text {id: 4} "world"
                                    ]
                                ])
