module Select exposing (..)

import Html exposing (Attribute, Html, option, select)
import Html.Attributes exposing (attribute, class, disabled, id, name, required, selected, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode


view :
    { value : String
    , required : Bool
    , disabled : Bool
    , ariaLabel : Maybe String
    , ariaDescribedBy : Maybe String
    , ariaInvalid : Bool
    , id : Maybe String
    , name : Maybe String
    , class : Maybe String
    , options : List ( String, String )
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
            , required config.required
            , disabled config.disabled
            , attribute "aria-invalid"
                (if config.ariaInvalid then
                    "true"

                 else
                    "false"
                )
            ]
                ++ List.filterMap identity
                    [ config.ariaLabel |> Maybe.map (\label -> attribute "aria-label" label)
                    , config.ariaDescribedBy |> Maybe.map (\desc -> attribute "aria-describedby" desc)
                    , config.id |> Maybe.map id
                    , config.name |> Maybe.map name
                    , config.onInput |> Maybe.map onInput
                    , config.onChange |> Maybe.map (\handler -> Html.Events.on "change" (Json.Decode.map handler Html.Events.targetValue))
                    , config.onBlur |> Maybe.map onBlur
                    , config.onFocus |> Maybe.map onFocus
                    ]

        optionElements =
            List.map (\( val, text ) -> option [ value val, selected (val == config.value) ] [ Html.text text ]) config.options
    in
    select attributes optionElements
