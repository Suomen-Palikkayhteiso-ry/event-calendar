port module Ports exposing (..)

import Json.Encode as Encode
import Types exposing (Auth)



-- Outgoing ports


port storeAuth : Auth -> Cmd msg


port removeAuth : () -> Cmd msg


port login : () -> Cmd msg


port logout : () -> Cmd msg


port initiateOAuth2Login : String -> Cmd msg


port handleOAuth2Callback : { code : String, state : Maybe String } -> Cmd msg


port parseKMLContent : String -> Cmd msg


port initMap : { center : ( Float, Float ), zoom : Int, events : List { id : String, title : String, point : Maybe { lat : Float, lon : Float } } } -> Cmd msg


port updateMap : { center : ( Float, Float ), zoom : Int, events : List { id : String, title : String, point : Maybe { lat : Float, lon : Float } } } -> Cmd msg



-- Incoming ports


port authStored : (Encode.Value -> msg) -> Sub msg


port authRemoved : (() -> msg) -> Sub msg


port kmlContentParsed : (Encode.Value -> msg) -> Sub msg


port mapMarkerMoved : (( Float, Float ) -> msg) -> Sub msg
