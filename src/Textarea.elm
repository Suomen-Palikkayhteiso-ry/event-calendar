module Textarea exposing (..)

import Html exposing (Attribute, Html, textarea)
import Html.Attributes exposing (attribute, class, cols, disabled, id, name, placeholder, readonly, required, rows, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode


view :
    { value : String
    , placeholder : Maybe String
    , required : Bool
    , disabled : Bool
    , readonly : Bool
    , rows : Maybe Int
    , cols : Maybe Int
    , ariaLabel : Maybe String
    , ariaDescribedBy : Maybe String
    , ariaInvalid : Bool
    , id : Maybe String
    , name : Maybe String
    , class : Maybe String
    , onInput : Maybe (String -> msg)
    , onChange : Maybe (String -> msg)
    , onBlur : Maybe msg
    , onFocus : Maybe msg
    }
    -> Html msg
view config =
    let
        baseClass =
            "form-input"

        fullClass =
            case config.class of
                Just c ->
                    baseClass ++ " " ++ c

                Nothing ->
                    baseClass

        attributes =
            [ class fullClass
            , value config.value
            , required config.required
            , disabled config.disabled
            , readonly config.readonly
            , attribute "aria-invalid"
                (if config.ariaInvalid then
                    "true"

                 else
                    "false"
                )
            ]
                ++ List.filterMap identity
                    [ config.placeholder |> Maybe.map placeholder
                    , config.rows |> Maybe.map rows
                    , config.cols |> Maybe.map cols
                    , config.ariaLabel |> Maybe.map (\label -> attribute "aria-label" label)
                    , config.ariaDescribedBy |> Maybe.map (\desc -> attribute "aria-describedby" desc)
                    , config.id |> Maybe.map id
                    , config.name |> Maybe.map name
                    , config.onInput |> Maybe.map onInput
                    , config.onChange |> Maybe.map (\handler -> Html.Events.on "change" (Json.Decode.map handler Html.Events.targetValue))
                    , config.onBlur |> Maybe.map onBlur
                    , config.onFocus |> Maybe.map onFocus
                    ]
    in
    textarea attributes []
