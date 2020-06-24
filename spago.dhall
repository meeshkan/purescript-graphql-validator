{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "graphql-validator"
, license = "Apache-2.0"
, repository = "https://github.com/meeshkan/purescript-graphql-validator"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "graphql-parser"
  , "profunctor-lenses"
  , "simple-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
