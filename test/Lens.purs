module Test.Lens where

import Prelude
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Data.GraphQL.Lens (getAllQueryDefinitions)
import Test.Spec (SpecT, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)
import Text.Parsing.Parser.String (class StringLike)
import Test.TwitterSchema (twitterSchema)

parseDocument ∷ ∀ s. StringLike s ⇒ s → Aff (AST.Document)
parseDocument t = liftEffect (either (throw <<< show) pure (runParser t GP.document))

testLens ∷ ∀ m. Monad m ⇒ SpecT Aff Unit m Unit
testLens =
  describe "tests twitter schema" do
    before (parseDocument twitterSchema)
      $ do
          it "gets the right number of query definitions" \doc → do
            let
              queries = getAllQueryDefinitions doc
            length queries `shouldEqual` 6
