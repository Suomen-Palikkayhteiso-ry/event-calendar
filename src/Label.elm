module Label exposing (..)

import Html exposing (Html, label, span, text)
import Html.Attributes exposing (attribute, class, for)


view :
    { htmlFor : Maybe String
    , required : Bool
    , children : List (Html msg)
    }
    -> Html msg
view config =
    label
        (List.filterMap identity
            [ config.htmlFor |> Maybe.map for
            , Just (class "form-label")
            ]
        )
        (config.children
            ++ (if config.required then
                    [ span [ class "text-error", attribute "aria-label" "required" ] [ text "*" ] ]

                else
                    []
               )
        )
