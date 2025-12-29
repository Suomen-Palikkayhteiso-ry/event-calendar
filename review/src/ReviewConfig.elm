module ReviewConfig exposing (config)

{-| ReviewConfig for elm-review
Reference: ADR-0010
-}

import NoDebug.Log
import NoDebug.TodoOrToString
import NoMissingTypeAnnotation
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoMissingTypeAnnotation.rule
    , NoSimpleLetBody.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    ]