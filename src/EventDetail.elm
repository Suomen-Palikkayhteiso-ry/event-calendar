module EventDetail exposing (Msg(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Types exposing (Event, Auth)
import DateUtils

type Msg
    = EditEvent String
    | DeleteEvent String
    | Back

view : Event -> Maybe Auth -> Html Msg
view event auth =
    div [ class "container mx-auto p-4" ]
        [ div [ class "flex justify-between items-start mb-6" ]
            [ h1 [ class "text-2xl font-bold" ] [ text event.title ]
            , case auth of
                Just _ ->
                    div [ class "flex gap-2" ]
                        [ button [ onClick (EditEvent event.id), class "btn btn-secondary" ] [ text "Edit" ]
                        , button [ onClick (DeleteEvent event.id), class "btn btn-danger" ] [ text "Delete" ]
                        ]
                Nothing ->
                    text ""
            ]
        , case event.image of
             Just imgPath ->
                 img [ src ("https://data.suomenpalikkayhteiso.fi/api/files/events/" ++ event.id ++ "/" ++ imgPath), class "w-full rounded-lg mb-6" ] []
             Nothing -> text ""
        , p [ class "mb-4 whitespace-pre-wrap" ] [ text (Maybe.withDefault "" event.description) ]
        , div [ class "grid gap-2" ]
             [ p [] [ strong [] [ text "Start: " ], text (DateUtils.formatDateInHelsinki event.startDate event.allDay) ]
             , p [] [ strong [] [ text "Location: " ], text (Maybe.withDefault "" event.location) ]
             , case event.url of
                 Just url -> p [] [ strong [] [ text "URL: " ], a [ href url, target "_blank" ] [ text url ] ]
                 Nothing -> text ""
             ]
        , button [ onClick Back, class "mt-8 inline-block text-blue-600 underline bg-transparent border-0 cursor-pointer" ] [ text "Back to Calendar" ]
        ]
