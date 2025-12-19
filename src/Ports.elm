port module Ports exposing (..)

import Json.Encode as Encode
import Types exposing (Auth)



-- Outgoing ports


port storeAuth : Auth -> Cmd msg


port removeAuth : () -> Cmd msg


port initMap : { center : ( Float, Float ), zoom : Int, marker : Maybe ( Float, Float ) } -> Cmd msg


port updateMap : { center : ( Float, Float ), zoom : Int, marker : Maybe ( Float, Float ) } -> Cmd msg


port parseKMLContent : String -> Cmd msg



-- Incoming ports


port authStored : (Auth -> msg) -> Sub msg


port authRemoved : (() -> msg) -> Sub msg


port mapMarkerMoved : (( Float, Float ) -> msg) -> Sub msg


port kmlContentParsed : (Encode.Value -> msg) -> Sub msg
