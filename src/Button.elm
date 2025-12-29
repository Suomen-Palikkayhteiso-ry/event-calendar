module Button exposing (..)

import Html exposing (Attribute, Html, button, text)
import Html.Attributes exposing (attribute, class, disabled, type_)
import Html.Events exposing (onClick)


type Variant
    = Primary
    | Secondary
    | Icon


type Size
    = Sm
    | Md
    | Lg


view :
    { variant : Variant
    , size : Size
    , disabled : Bool
    , type_ : String
    , ariaLabel : Maybe String
    , title : Maybe String
    , onClick : Maybe msg
    , children : List (Html msg)
    }
    -> Html msg
view config =
    let
        variantClass =
            case config.variant of
                Primary ->
                    "btn-primary"

                Secondary ->
                    "btn-secondary"

                Icon ->
                    "btn-icon"

        sizeClass =
            case config.size of
                Sm ->
                    "btn-sm"

                Md ->
                    "btn-md"

                Lg ->
                    "btn-lg"

        disabledClass =
            if config.disabled then
                "disabled"

            else
                ""

        classes =
            "btn " ++ variantClass ++ " " ++ sizeClass ++ " " ++ disabledClass

        attributes =
            [ class classes
            , type_ config.type_
            , Html.Attributes.disabled config.disabled
            ]
                ++ (case config.ariaLabel of
                        Just label ->
                            [ attribute "aria-label" label ]

                        Nothing ->
                            []
                   )
                ++ (case config.title of
                        Just t ->
                            [ Html.Attributes.title t ]

                        Nothing ->
                            []
                   )
                ++ (case config.onClick of
                        Just msg ->
                            [ Html.Events.onClick msg ]

                        Nothing ->
                            []
                   )
    in
    button attributes config.children
