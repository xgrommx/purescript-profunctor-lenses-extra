{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "argonaut"
    , "assert"
    , "console"
    , "effect"
    , "filterable"
    , "foldable-traversable"
    , "generics-rep"
    , "profunctor-lenses"
    , "psci-support"
    , "spec"
    , "these"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
