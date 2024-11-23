module Test.Vi where

import Prelude
import Effect (Effect)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter (consoleReporter)
import Test.Vi.Model.InputMethod as InputMethod

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    InputMethod.spec
