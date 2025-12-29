module Main exposing (..)

import Browser
import Model
import Update
import View


main : Program (Maybe String) Model.Model Update.Msg
main =
    Browser.application
        { init = \flags url key -> let (m, c) = Model.init flags url key in (m, Cmd.map Update.EventsMsg c)
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        , onUrlChange = Update.UrlChanged
        , onUrlRequest = Update.LinkClicked
        }


subscriptions : Model.Model -> Sub Update.Msg
subscriptions _ =
    Sub.none
