port module Ports exposing (..)module Ports exposing (..)

import Types exposing (Auth)


-- Outgoing ports


port storeAuth : Auth -> Cmd msg


port removeAuth : () -> Cmd msg


-- Incoming ports


port authStored : (Auth -> msg) -> Sub msg


port authRemoved : (() -> msg) -> Sub msg