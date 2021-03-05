{ name =
    "grasp"
, dependencies =
    [ "argonaut-core"
    , "console"
    , "debug"
    , "effect"
    , "filterable"
    , "parsing"
    , "profunctor-lenses"
    , "psci-support"
    , "spec"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
