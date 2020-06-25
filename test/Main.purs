module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Lens (testLens)
import Test.RequestValidator (testRequestValidator)
import Test.ResponseValidator (testResponseValidator)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        testResponseValidator
        testRequestValidator
        testLens
