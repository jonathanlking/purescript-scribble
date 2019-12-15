{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "scribble"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "effect"
    , "foreign"
    , "generics-rep"
    , "indexed-monad"
    , "prelude"
    , "psci-support"
    , "profunctor-lenses"
    , "argonaut"
    , "web-socket"
    , "web-events"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
