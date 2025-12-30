module View exposing (view)

import Browser
import Calendar
import Html exposing (Html, a, button, div, footer, h1, h2, nav, p, text)
import Html.Attributes exposing (class, href)
import I18n
import Map
import Model
import Routes
import Update


view : Model.Model -> Browser.Document Update.Msg
view model =
    { title = I18n.get "event_calendar"
    , body =
        [ div [ class "min-h-screen flex flex-col" ]
            [ headerView
            , mainView model
            , footerView
            ]
        ]
    }


headerView : Html Update.Msg
headerView =
    nav [ class "bg-white shadow-sm border-b border-gray-200" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
            [ div [ class "flex justify-between h-16" ]
                [ div [ class "flex" ]
                    [ div [ class "flex-shrink-0 flex items-center" ]
                        [ h1 [ class "text-xl font-bold text-gray-900" ] [ text (I18n.get "event_calendar") ] ]
                    , div [ class "hidden sm:ml-6 sm:flex sm:space-x-8" ]
                        [ navLink "/" "calendar"
                        , navLink "/events" "events"
                        , navLink "/map" "map"
                        ]
                    ]
                ]
            ]
        ]


navLink : String -> String -> Html Update.Msg
navLink url label =
    a [ href url, class "border-transparent text-gray-500 hover:border-gray-300 hover:text-gray-700 inline-flex items-center px-1 pt-1 border-b-2 text-sm font-medium" ] [ text (I18n.get label) ]


mainView : Model.Model -> Html Update.Msg
mainView model =
    div [ class "flex-1" ]
        [ case model.route of
            Routes.Home ->
                div []
                    [ case model.auth of
                        Nothing ->
                            loginInstructions
                        
                        Just _ ->
                            text ""
                    , Html.map Update.CalendarMsg (Calendar.view model.calendar)
                    ]

            Routes.EventsRoute ->
                text "Event List View"

            Routes.MapRoute ->
                Html.map Update.MapMsg (Map.view model.map)

            Routes.EventDetail id ->
                text ("Event Detail: " ++ id)

            Routes.EditEvent id ->
                text ("Edit Event: " ++ id)

            Routes.CreateEvent ->
                text "Create Event View"

            Routes.Callback ->
                text "Callback"

            Routes.NotFound ->
                text "Not Found"
        ]


loginInstructions : Html msg
loginInstructions =
    div [ class "max-w-md mx-auto mt-8 p-6 bg-white rounded-lg shadow-md" ]
        [ h2 [ class "text-xl font-semibold mb-4" ] [ text "Welcome to the Event Calendar" ]
        , p [ class "mb-4" ] [ text "To view and manage events, please log in as a member." ]
        , p [ class "mb-4" ] [ text "If you don't have an account, contact an administrator to become a member." ]
        , button [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded" ] [ text "Login" ]
        ]


footerView : Html msg
footerView =
    footer [ class "bg-gray-50 border-t border-gray-200 mt-auto" ]
        [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
            [ div [ class "flex flex-col sm:flex-row justify-between items-center" ]
                [ div [ class "text-sm text-gray-500 mb-4 sm:mb-0" ]
                    [ text (I18n.get "feeds_description") ]
                , div [ class "flex space-x-6" ]
                    [ a
                        [ href "/feeds/rss.xml"
                        , class "text-sm text-gray-600 hover:text-gray-900"
                        ]
                        [ text (I18n.get "rss_feed") ]
                    , a
                        [ href "/feeds/atom.xml"
                        , class "text-sm text-gray-600 hover:text-gray-900"
                        ]
                        [ text (I18n.get "atom_feed") ]
                    , a
                        [ href "/feeds/ical.ics"
                        , class "text-sm text-gray-600 hover:text-gray-900"
                        ]
                        [ text (I18n.get "ical_feed") ]
                    , a
                        [ href "/feeds/geojson.json"
                        , class "text-sm text-gray-600 hover:text-gray-900"
                        ]
                        [ text (I18n.get "geojson_feed") ]
                    ]
                ]
            ]
        ]
