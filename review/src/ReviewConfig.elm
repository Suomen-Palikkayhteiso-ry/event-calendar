module ReviewConfig exposing (config)

{-| ReviewConfig for elm-review
Reference: ADR-0010
-}

import NoUnused.Variables
import NoUnused.Modules
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnused.Variables.rule
    , NoUnused.Modules.rule
    ]
