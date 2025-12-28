module ReviewConfig exposing (config)

{-| ReviewConfig for elm-review
Reference: ADR-0010
-}

import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Review.Rule.NoUnusedVariables.rule
    , Review.Rule.NoUnusedModules.rule
    , Review.Rule.NoExposingEverything.rule
    ]