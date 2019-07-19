{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "argonaut"
    , "console"
    , "effect"
    , "filterable"
    , "foldable-traversable"
    , "generics-rep"
    , "profunctor-lenses"
    , "psci-support"
    , "variant"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
