module Modal exposing (..)

import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (attribute, class, tabindex)
import Html.Events exposing (onClick)


view :
    { open : Bool
    , title : Maybe String
    , size : String
    , onClose : Maybe msg
    , children : List (Html msg)
    }
    -> Html msg
view config =
    if config.open then
        div
            [ class "modal-backdrop"
            , attribute "role" "dialog"
            , attribute "aria-modal" "true"
            , tabindex -1
            , onClick (config.onClose |> Maybe.withDefault (always ()))
            ]
            [ div
                [ class ("modal-content modal-" ++ config.size)
                , onClick (always ()) -- prevent close on content click
                ]
                (case config.title of
                    Just t ->
                        [ div [ class "modal-header" ]
                            [ h2 [ class "modal-title", attribute "id" "modal-title" ] [ text t ]
                            , button
                                [ class "modal-close"
                                , attribute "aria-label" "Close modal"
                                , onClick (config.onClose |> Maybe.withDefault (always ()))
                                ]
                                [ text "×" ]
                            ]
                        , div [ class "modal-body" ] config.children
                        ]

                    Nothing ->
                        [ div [ class "modal-body" ] config.children ]
                )
            ]

    else
        text ""
