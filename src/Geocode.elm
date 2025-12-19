module Geocode exposing (..)

import Http
import Json.Decode as Decode
import Url.Builder as Url


type alias Coordinates =
    ( Float, Float )


geocodeLocation : String -> (Result Http.Error (Maybe Coordinates) -> msg) -> Cmd msg
geocodeLocation location toMsg =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org"
                [ "search" ]
                [ Url.string "q" location
                , Url.string "format" "json"
                , Url.string "limit" "1"
                ]

        expect =
            Http.expectJson toMsg (Decode.list resultDecoder |> Decode.map List.head)
    in
    Http.get
        { url = url
        , expect = expect
        }


resultDecoder : Decode.Decoder Coordinates
resultDecoder =
    Decode.map2 (\lat lon -> ( lat, lon ))
        (Decode.field "lat" Decode.string |> Decode.andThen (String.toFloat >> maybeToDecoder))
        (Decode.field "lon" Decode.string |> Decode.andThen (String.toFloat >> maybeToDecoder))


maybeToDecoder : Maybe a -> Decode.Decoder a
maybeToDecoder maybe =
    case maybe of
        Just value ->
            Decode.succeed value

        Nothing ->
            Decode.fail "Invalid number"


reverseGeocode : Float -> Float -> (Result Http.Error (Maybe String) -> msg) -> Cmd msg
reverseGeocode lat lng toMsg =
    let
        url =
            Url.crossOrigin "https://nominatim.openstreetmap.org"
                [ "reverse" ]
                [ Url.string "lat" (String.fromFloat lat)
                , Url.string "lon" (String.fromFloat lng)
                , Url.string "format" "json"
                ]

        expect =
            Http.expectJson toMsg (Decode.maybe (Decode.field "display_name" Decode.string))
    in
    Http.get
        { url = url
        , expect = expect
        }
