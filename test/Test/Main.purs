module Test.Main.Strom where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Yoga.Om.Strom.Test as StromSpec

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  StromSpec.spec
