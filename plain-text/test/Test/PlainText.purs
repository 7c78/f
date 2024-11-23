module Test.PlainText where

import Prelude
import Effect (Effect)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter (consoleReporter)
import Test.PlainText.Model.NodeWithSelection as Test.NodeWithSelection
import Test.PlainText.Model.Selection as Model.Selection
import Test.PlainText.Model.Splice.Internal as Model.Splice
import Test.PlainText.Model.NodeOperation.Internal as Model.NodeOperation
import Test.PlainText.View.Diff as View.Diff

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    Test.NodeWithSelection.spec
    Model.Selection.spec
    Model.Splice.spec
    Model.NodeOperation.spec
    View.Diff.spec
