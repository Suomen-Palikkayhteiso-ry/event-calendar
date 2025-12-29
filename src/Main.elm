module Main exposing (..)

import Browser
import Map
import Model
import Ports
import Update
import View


main : Program () Model.Model Update.Msg
main =
    Browser.application
        { init =
            \() url key ->
                let
                    ( m, c ) =
                        Model.init Nothing url key
                in
                ( m, Cmd.map Update.EventsMsg c )
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        , onUrlChange = Update.UrlChanged
        , onUrlRequest = Update.LinkClicked
        }


subscriptions : Model.Model -> Sub Update.Msg
subscriptions _ =
    Ports.mapMarkerMoved (\pos -> Update.MapMsg (Map.MarkerMoved pos))
