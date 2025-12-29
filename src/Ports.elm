port module Ports exposing (..)

import Json.Encode as Encode
import Types exposing (Auth)



-- Outgoing ports


port storeAuth : Auth -> Cmd msg


port removeAuth : () -> Cmd msg


port initiateOAuth2Login : String -> Cmd msg


port handleOAuth2Callback : { code : String, state : Maybe String } -> Cmd msg


port parseKMLContent : String -> Cmd msg



-- Incoming ports


port authStored : (Encode.Value -> msg) -> Sub msg


port authRemoved : (() -> msg) -> Sub msg


port kmlContentParsed : (Encode.Value -> msg) -> Sub msg
