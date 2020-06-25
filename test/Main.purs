module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Lens (testLens)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.ResponseValidator (testValidator)

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        testValidator
        testLens
