module PlainText.App.Main where

import Prelude
import Effect (Effect)
import FFI.DOM.Document (getElementById)
import PlainText.Model.Node (Node(..))
import PlainText.View.Editor (makeEditor)
import Test.PlainText.Model.Node (p, text)

main :: Effect Unit
main = do
    rootDOM <- getElementById "editor-1"
    makeEditor rootDOM sample0

sample0 :: Node
sample0 =
    Root {id: 1}
        [ p {id: 2}
            [ text {id: 3} "hello world"
            ]
        ]
