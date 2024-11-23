module Test.PlainText.Model.Selection where

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.PlainText.Model.NodeWithSelection (br, node, p, root, selection, text)
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Unsafe ((&))
import Data.List.Unsafe (arrayList)
import PlainText.Model.Selection (nearestPointBackward, nearestPointForward)

spec :: Spec Unit
spec = do
    describe "PlainText.Model.Selection" do
        describe "nearestPointBackward" do
            it "at start" do
                let r = root {id: 0}
                        [ p {id: 1}
                            [ text {id: 2} "|foo"
                            ]
                        ]
                    cursor & _ = selection r

                nearestPointBackward (node r) cursor `shouldEqual`
                    Nothing

            it "in a block" do
                let r = root {id: 0}
                        [ p {id: 1}
                            [ text {id: 2} "foo"
                            , br {id: 3}
                            , text {id: 4} "|baz"
                            ]
                        ]
                    cursor & _ = selection r

                nearestPointBackward (node r) cursor `shouldEqual`
                    Just (arrayList [0, 0] & 3)

            it "across blocks" do
                let r = root {id: 0}
                            [ p {id: 1}
                                [ text {id: 2} "hello"
                                , text {id: 3} "foo"
                                ]
                            , p {id: 4}
                                [ text {id: 5} "|text 5"
                                ]
                            ]
                    cursor & _ = selection r

                nearestPointBackward (node r) cursor `shouldEqual`
                    Just (arrayList [0, 1] & 3)

        describe "nearestPointForward" do
            it "at end" do
                let r = root {id: 0}
                        [ p {id: 1}
                            [ text {id: 2} "foo|"
                            ]
                        ]
                    cursor & _ = selection r

                nearestPointForward (node r) cursor `shouldEqual`
                    Nothing

            it "in a block" do
                let r = root {id: 0}
                        [ p {id: 1}
                            [ text {id: 2} "foo|"
                            , br {id: 3}
                            , text {id: 4} "baz"
                            ]
                        ]
                    cursor & _ = selection r

                nearestPointForward (node r) cursor `shouldEqual`
                    Just (arrayList [0, 2] & 0)

            it "across blocks" do
                let r = root {id: 0}
                            [ p {id: 1}
                                [ text {id: 2} "hello"
                                , text {id: 3} "foo|"
                                ]
                            , p {id: 4}
                                [ text {id: 5} "text 5"
                                ]
                            ]
                    cursor & _ = selection r

                nearestPointForward (node r) cursor `shouldEqual`
                    Just (arrayList [1, 0] & 0)
