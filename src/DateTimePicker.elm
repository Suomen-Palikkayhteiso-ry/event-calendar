module DateTimePicker exposing (..)

import Html exposing (Html, input)
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
    if config.allDay then
        input
            [ type_ "date"
            , id config.id
            , value config.value
            , disabled config.disabled
            , onInput config.onChange
            , class "form-input"
            ]
            []

      else
        input
            [ type_ "datetime-local"
            , id config.id
            , value config.value
            , disabled config.disabled
            , onInput config.onChange
            , class "form-input"
            ]
            []
