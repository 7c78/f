module Test.PlainText.View.Diff where

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Prelude
import Data.Set as Set
import PlainText.View.Diff (EditOperation(..), diff)
import Test.PlainText.Model.Node (h1, p, text, id)

spec :: Spec Unit
spec = do
    describe "PlainText.View.Diff" do
        describe "diff" do
            it "insert all" do
                let new = [ h1 {id: 1}
                              [ text {id: 2} "hello"
                              ]
                          , p {id: 3}
                              [ text {id: 4} "world"
                              ]
                          ]
                    old = []
                diff old new Set.empty `shouldEqual`
                    [ Insert (new `id` 1)
                    , Insert (new `id` 3)
                    ]

            it "delete all" do
                let new = []
                    old = [1, 2, 3]
                diff old new Set.empty `shouldEqual`
                    [ Delete 1
                    , Delete 2
                    , Delete 3
                    ]

            it "replace" do
                let new = [ h1 {id: 1} []
                          , p {id: 2} []
                          , p {id: 3} []
                          , p {id: 6} []
                          , p {id: 5} []
                          ]
                    old = [1, 2, 3, 4, 5]
                diff old new Set.empty `shouldEqual`
                    [ Retain 1
                    , Retain 2
                    , Retain 3
                    , Replace 4 (new `id` 6)
                    , Retain 5
                    ]
