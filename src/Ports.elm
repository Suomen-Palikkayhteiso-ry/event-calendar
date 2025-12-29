port module Ports exposing (..)

-- Outgoing ports


port login : () -> Cmd msg


port logout : () -> Cmd msg


port handleOAuth2Callback : { code : String, state : Maybe String } -> Cmd msg


port initMap : { center : ( Float, Float ), zoom : Int, events : List { id : String, title : String, point : Maybe { lat : Float, lon : Float } } } -> Cmd msg


port updateMap : { center : ( Float, Float ), zoom : Int, events : List { id : String, title : String, point : Maybe { lat : Float, lon : Float } } } -> Cmd msg



-- Incoming ports


port mapMarkerMoved : (( Float, Float ) -> msg) -> Sub msg
