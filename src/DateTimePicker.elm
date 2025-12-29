module DateTimePicker exposing (..)

import Html exposing (Html, input)
import Html.Attributes exposing (class, disabled, id, placeholder, type_, value)
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
            [ type_ "text"
            , id config.id
            , value (formatFinnishDate config.value)
            , disabled config.disabled
            , onInput (parseFinnishDate >> config.onChange)
            , placeholder "p.k.vvvv (esim. 25.12.2024)"
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


formatFinnishDate : String -> String
formatFinnishDate isoString =
    if String.isEmpty isoString then
        ""

    else
        -- Parse ISO date YYYY-MM-DDTHH:MM:SSZ to Finnish format DD.MM.YYYY
        case String.split "T" isoString of
            date :: _ ->
                case String.split "-" date of
                    [ year, month, day ] ->
                        day ++ "." ++ month ++ "." ++ year

                    _ ->
                        isoString

            _ ->
                isoString


parseFinnishDate : String -> String
parseFinnishDate finnishDate =
    if String.isEmpty finnishDate then
        ""

    else
        -- Parse Finnish date DD.MM.YYYY to ISO format YYYY-MM-DDTHH:MM:SSZ
        case String.split "." finnishDate of
            [ day, month, year ] ->
                case ( String.toInt day, String.toInt month, String.toInt year ) of
                    ( Just d, Just m, Just y ) ->
                        String.padLeft 4 '0' (String.fromInt y)
                            ++ "-"
                            ++ String.padLeft 2 '0' (String.fromInt m)
                            ++ "-"
                            ++ String.padLeft 2 '0' (String.fromInt d)
                            ++ "T00:00:00Z"

                    _ ->
                        finnishDate

            _ ->
                finnishDate
