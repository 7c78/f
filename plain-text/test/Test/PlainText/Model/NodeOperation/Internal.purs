module Test.PlainText.Model.NodeOperation.Internal where

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.PlainText.Model.Node (br, p, root, text)
import Prelude
import Data.List.Unsafe (arrayList)
import PlainText.Model.NodeOperation.Internal (deleteNode, insertNode, joinTexts)

spec :: Spec Unit
spec = do
    describe "PlainText.Model.NodeOperation.Internal" do
        describe "insertNode" do
            it "insert a node at path" do
                let r = root {id: 1}
                            [ p {id: 2}
                                [ text {id: 3} "foo"
                                , text {id: 4} "bar"
                                ]
                            ]
                insertNode (arrayList [0, 1]) (br {id: 0}) r `shouldEqual`
                    root {id: 1}
                        [ p {id: 2}
                            [ text {id: 3} "foo"
                            , br {id: 0}
                            , text {id: 4} "bar"
                            ]
                        ]

        describe "deleteNode" do
            it "delete a node at path" do
                let r = root {id: 1}
                        [ p {id: 2}
                            [ text {id: 3} "foo"
                            , br {id: 0}
                            , text {id: 4} "bar"
                            ]
                        ]
                deleteNode (arrayList [0, 1]) r `shouldEqual`
                    root {id: 1}
                        [ p {id: 2}
                            [ text {id: 3} "foo"
                            , text {id: 4} "bar"
                            ]
                        ]

        describe "joinTexts" do
            it "join texts" do
                let r = root {id: 1}
                            [ p {id: 2}
                                [ text {id: 3} "foo"
                                , text {id: 4} "bar"
                                , text {id: 5} "baz"
                                ]
                            ]
                joinTexts (arrayList [0]) 0 2 r `shouldEqual`
                    root {id: 1}
                        [ p {id: 2}
                            [ text {id: 3} "foobarbaz"
                            ]
                        ]
