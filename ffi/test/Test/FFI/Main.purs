module Test.FFI.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter (consoleReporter)
import Test.FFI.AssocArray as AssocArray

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
    AssocArray.spec
