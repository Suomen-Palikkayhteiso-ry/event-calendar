module Geocode exposing (..)

import Json.Decode as Decode


type alias Coordinates =
    ( Float, Float )


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
