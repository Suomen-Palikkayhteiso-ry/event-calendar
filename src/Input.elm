module Input exposing (..)

import Html exposing (Attribute, Html, input)
import Html.Attributes exposing (accept, attribute, autofocus, class, disabled, id, max, min, name, pattern, placeholder, readonly, required, step, type_, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode


view :
    { type_ : String
    , value : String
    , placeholder : Maybe String
    , required : Bool
    , disabled : Bool
    , readonly : Bool
    , ariaLabel : Maybe String
    , ariaDescribedBy : Maybe String
    , ariaInvalid : Bool
    , ariaRequired : Bool
    , id : Maybe String
    , name : Maybe String
    , pattern : Maybe String
    , min : Maybe String
    , max : Maybe String
    , step : Maybe String
    , accept : Maybe String
    , autofocus : Bool
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
            [ type_ config.type_
            , value config.value
            , class fullClass
            , required config.required
            , disabled config.disabled
            , readonly config.readonly
            , autofocus config.autofocus
            , attribute "aria-invalid" (if config.ariaInvalid then "true" else "false")
            , attribute "aria-required" (if config.ariaRequired then "true" else "false")
            ]
                ++ List.filterMap identity
                    [ config.placeholder |> Maybe.map placeholder
                    , config.ariaLabel |> Maybe.map (\label -> attribute "aria-label" label)
                    , config.ariaDescribedBy |> Maybe.map (\desc -> attribute "aria-describedby" desc)
                    , config.id |> Maybe.map id
                    , config.name |> Maybe.map name
                    , config.pattern |> Maybe.map pattern
                    , config.min |> Maybe.map Html.Attributes.min
                    , config.max |> Maybe.map Html.Attributes.max
                    , config.step |> Maybe.map step
                    , config.accept |> Maybe.map accept
                    , config.onInput |> Maybe.map onInput
                    , config.onChange |> Maybe.map (\handler -> Html.Events.on "change" (Json.Decode.map handler Html.Events.targetValue))
                    , config.onBlur |> Maybe.map onBlur
                    , config.onFocus |> Maybe.map onFocus
                    ]
    in
    input attributes []