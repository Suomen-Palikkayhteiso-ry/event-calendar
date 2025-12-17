module DateTimePicker exposing (..)module DateTimePicker exposing (..)

import Html exposing (Attribute, Html, div, input, label)
import Html.Attributes exposing (class, disabled, id, type_, value)
import Html.Events exposing (onInput)


view :
    { value : String
    , label : String
    , id : String
    , disabled : Bool
    , allDay : Bool
    , onChange : String -> msg
    }
    -> Html msg
view config =
    div [ class "mb-4" ]
        [ label [ class "mb-2 block font-medium text-gray-700", Html.Attributes.for config.id ] [ Html.text config.label ]
        , input
            [ type_ "datetime-local"
            , id config.id
            , value config.value
            , disabled config.disabled
            , onInput config.onChange
            , class "form-input"
            ]
            []
        ]