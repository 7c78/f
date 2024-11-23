module Test.PlainText.Model.Splice.Internal where

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.PlainText.Model.NodeWithSelection (br, h2, link, link', node, p, root, selection, text)
import Prelude
import Data.Tuple.Unsafe ((&))
import PlainText.Model.Node (maximumNodeId)
import PlainText.Model.Splice.Internal (splice, spliceBlock, spliceInline, spliceText)

spec :: Spec Unit
spec = do
    describe "PlainText.Model.Splice.Internal" do
        describe "splice" do
            it "in a text node" do
                let r = p {id: 1}
                            [ text {id: 2} "w|o|rld"
                            , text {id: 3} "foo"
                            ]
                    start & end = selection r

                splice start end (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "w|rld"
                                , text {id: 3} "foo"
                                ])

            it "in a block" do
                let r = p {id: 1}
                            [ text {id: 2} "f|oo"
                            , text {id: 3} "hello"
                            , text {id: 4} "wo|rld"
                            , text {id: 5} "bar"
                            ]
                    start & end = selection r

                splice start end (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "f|rld"
                                , text {id: 5} "bar"
                                ])

            it "across blocks" do
                let r = root {id: 0}
                            [ p {id: 1}
                                [ text {id: 2} "hello"
                                , text {id: 3} "f|oo"
                                ]
                            , p {id: 4}
                                [ text {id: 5} "text 5"
                                ]
                            , h2 {id: 6}
                                [ text {id: 7} "ba|r"
                                , text {id: 8} "world"
                                ]
                            ]
                    start & end = selection r

                splice start end (node r) `shouldEqual`
                    (node $ root {id: 0}
                                [ p {id: 1}
                                    [ text {id: 2} "hello"
                                    , text {id: 3} "f|r"
                                    , text {id: 8} "world"
                                    ]
                                ])

            it "join compatible inline nodes" do
                let r = p {id: 1}
                            [ link' {id: 2} {href: "example.com"}
                                [ text {id: 3} "fo|o"
                                , text {id: 4} "bar"
                                ]
                            , text {id: 5} "baz"
                            , link' {id: 6} {href: "example.com"}
                                [ text {id: 7} "text|7"
                                ]
                            ]
                    start & end = selection r

                splice start end (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ link' {id: 2} {href: "example.com"}
                                    [ text {id: 3} "fo|7"
                                    ]
                                ])

            it "do not join incompatible nodes" do
                let r = p {id: 1}
                            [ link {id: 2}
                                [ text {id: 3} "fo|o"
                                , text {id: 4} "bar"
                                ]
                            , text {id: 5} "b|az"
                            ]
                    start & end = selection r

                splice start end (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ link {id: 2}
                                    [ text {id: 3} "fo|"
                                    ]
                                , text {id: 5} "az"
                                ])

        describe "spliceText" do
            it "in a text node" do
                let r = p {id: 1}
                            [ text {id: 2} "h|ello"
                            ]
                    start & end = selection r

                spliceText start end "X" (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "hXello"
                                ])

            it "in a block" do
                let r = p {id: 1}
                            [ text {id: 2} "f|oo"
                            , text {id: 3} "hello"
                            , text {id: 4} "wo|rld"
                            , text {id: 5} "bar"
                            ]
                    start & end = selection r

                spliceText start end "X" (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "fXrld"
                                , text {id: 5} "bar"
                                ])

            it "across blocks" do
                let r = root {id: 0}
                            [ p {id: 1}
                                [ text {id: 2} "|foo"
                                ]
                            , p {id: 3}
                                [ text {id: 4} "b|ar"
                                , br {id: 5}
                                , text {id: 6} "baz"
                                ]
                            ]
                    start & end = selection r

                spliceText start end "X" (node r) `shouldEqual`
                    (node $ root {id: 0}
                                [ p {id: 1}
                                    [ text {id: 2} "Xar"
                                    , br {id: 5}
                                    , text {id: 6} "baz"
                                    ]
                                ])

        describe "spliceInline" do
            it "insert a line break" do
                let r = p {id: 1}
                            [ text {id: 2} "f|oo"
                            , text {id: 3} "ba|r"
                            , text {id: 4} "baz"
                            ]
                    start & end = selection r
                    id = maximumNodeId (node r) + 1

                spliceInline start end (node $ br {id}) (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "f"
                                , br {id: 5}
                                , text {id: 3} "r"
                                , text {id: 4} "baz"
                                ])

            it "insert a line break -- split" do
                let r = p {id: 1}
                            [ text {id: 2} "foo"
                            , text {id: 3} "ba|r"
                            , text {id: 4} "baz"
                            ]
                    start & end = selection r
                    id = maximumNodeId (node r) + 1

                spliceInline start end (node $ br {id}) (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "foo"
                                , text {id: 3} "ba"
                                , br {id: 5}
                                , text {id: 6} "r"
                                , text {id: 4} "baz"
                                ])

            it "insert a line break -- split & nested" do
                let r = p {id: 1}
                            [ text {id: 2} "hello"
                            , link {id: 3}
                                [ text {id: 5} "f|o|o"
                                , text {id: 6} "bar"
                                ]
                            , text {id: 7} "baz"
                            ]
                    start & end = selection r
                    id = maximumNodeId (node r) + 1

                spliceInline start end (node $ br {id}) (node r) `shouldEqual`
                    (node $ p {id: 1}
                                [ text {id: 2} "hello"
                                , link {id: 3}
                                    [ text {id: 5} "f"
                                    ]
                                , br {id: 8}
                                , link {id: 10}
                                    [ text {id: 9} "o"
                                    , text {id: 6} "bar"
                                    ]
                                , text {id: 7} "baz"
                                ])

        describe "spliceBlock" do
            it "insert a paragraph -- cursor middle" do
                let r = root {id: 1}
                            [ p {id: 2}
                                [ text {id: 3} "f|oo"
                                ]
                            ]
                    start & end = selection r
                    id = maximumNodeId (node r) + 1

                spliceBlock start end (node $ p {id} []) (node r) `shouldEqual`
                    (node $ root {id: 1}
                                [ p {id: 2}
                                    [ text {id: 3} "f"
                                    ]
                                , p {id: 4}
                                    [ text {id: 5} "oo"
                                    ]
                                ])

            it "insert a paragraph -- cursor end" do
                let r = root {id: 1}
                            [ p {id: 2}
                                [ text {id: 3} "foo|"
                                ]
                            ]
                    start & end = selection r
                    id = maximumNodeId (node r) + 1

                spliceBlock start end (node $ p {id} []) (node r) `shouldEqual`
                    (node $ root {id: 1}
                                [ p {id: 2}
                                    [ text {id: 3} "foo"
                                    ]
                                , p {id: 4}
                                    [ text {id: 5} ""
                                    ]
                                ])
