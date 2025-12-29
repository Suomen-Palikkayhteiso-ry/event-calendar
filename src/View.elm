module View exposing (view)

import Browser
import Calendar
import DateTimePicker
import EventDetail
import EventForm
import EventList
import Html exposing (a, button, div, h1, header, label, main_, nav, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18n
import Icons
import Map
import Model exposing (Model)
import Routes
import Update


view : Model -> Browser.Document Update.Msg
view model =
    { title = I18n.get "event_calendar"
    , body =
        [ div [ class "min-h-screen flex flex-col" ]
            [ header [ class "bg-white shadow-sm border-b border-gray-200 sticky top-0 z-50" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex justify-between items-center h-12" ]
                        [ div [ class "flex items-center space-x-4" ]
                            [ h1 [ class "text-lg font-semibold text-gray-900" ] [ text (I18n.get "event_calendar") ]
                            , nav [ class "flex space-x-8" ]
                                [ a [ href "#/", class "text-gray-900 hover:text-blue-600 px-3 py-2 text-sm font-medium", attribute "aria-label" (I18n.get "home") ] [ Icons.homeIcon ]
                                , if model.auth /= Nothing then
                                    a [ href "#/events", class "text-gray-500 hover:text-blue-600 px-3 py-2 text-sm font-medium", attribute "aria-label" (I18n.get "events") ] [ Icons.calendarIcon ]

                                  else
                                    text ""
                                , a [ href "#/map", class "text-gray-500 hover:text-blue-600 px-3 py-2 text-sm font-medium", attribute "aria-label" (I18n.get "map") ] [ Icons.mapIcon ] -- Add Map link
                                ]
                            ]
                        , div [ class "flex items-center space-x-4" ]
                            [ case model.auth of
                                Just auth ->
                                    let
                                        displayName =
                                            case auth.user of
                                                Just user ->
                                                    let
                                                        name =
                                                            user.name
                                                                |> Maybe.andThen
                                                                    (\n ->
                                                                        if String.isEmpty n then
                                                                            Nothing

                                                                        else
                                                                            Just n
                                                                    )

                                                        username =
                                                            user.username
                                                                |> Maybe.andThen
                                                                    (\u ->
                                                                        if String.isEmpty u then
                                                                            Nothing

                                                                        else
                                                                            Just u
                                                                    )
                                                    in
                                                    case name of
                                                        Just n ->
                                                            n

                                                        Nothing ->
                                                            case username of
                                                                Just u ->
                                                                    u

                                                                Nothing ->
                                                                    user.email

                                                Nothing ->
                                                    "User"
                                    in
                                    button [ class "text-gray-700 hover:text-gray-900 px-3 py-2 text-sm font-medium", onClick Update.Logout, attribute "aria-label" (I18n.get "logout"), title displayName ] [ Icons.logoutIcon ]

                                Nothing ->
                                    button [ class "text-gray-700 hover:text-gray-900 px-3 py-2 text-sm font-medium", onClick Update.Login, attribute "aria-label" (I18n.get "login") ] [ Icons.loginIcon ]
                            ]
                        ]
                    ]
                ]
            , main_ [ class "flex-1" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6" ]
                    [ if model.loading then
                        div [ class "flex justify-center items-center h-64" ]
                            [ div [ class "animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600" ] []
                            ]

                      else
                        case model.route of
                                Routes.Home ->
                                    div [ class "space-y-4" ]
                                        [ div [ class "bg-white shadow rounded-lg" ]
                                            [ div [ class "px-3 py-2 sm:p-3" ]
                                                [ case model.auth of
                                                    Just _ ->
                                                        div [ class "mb-4 flex items-center justify-between" ]
                                                            [ div [ class "flex items-center space-x-4" ]
                                                                [ label [ class "block text-sm font-medium text-gray-700" ] [ text (I18n.get "select_date") ]
                                                                , div [ class "relative" ]
                                                                    [ DateTimePicker.view
                                                                        { value = model.selectedDate
                                                                        , label = ""
                                                                        , id = "calendar-date"
                                                                        , disabled = False
                                                                        , allDay = True
                                                                        , onChange = Update.SetDate
                                                                        }
                                                                    , div [ class "mt-1 text-sm text-gray-500" ] [ text "Anna päivämäärä muodossa p.k.vvvv" ]
                                                                    ]
                                                                ]
                                                            , button [ class "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500", onClick Update.GoToCreateEvent, title "Add new event" ]
                                                                [ text ("+ " ++ I18n.get "add_new_event") ]
                                                            ]

                                                    Nothing ->
                                                        div [ class "mb-4 flex items-center justify-between" ]
                                                            [ div [ class "flex items-center space-x-4" ]
                                                                [ label [ class "block text-sm font-medium text-gray-700" ] [ text (I18n.get "select_date") ]
                                                                , div [ class "relative" ]
                                                                    [ DateTimePicker.view
                                                                        { value = model.selectedDate
                                                                        , label = ""
                                                                        , id = "calendar-date"
                                                                        , disabled = False
                                                                        , allDay = True
                                                                        , onChange = Update.SetDate
                                                                        }
                                                                    , div [ class "mt-1 text-sm text-gray-500" ] [ text "Anna päivämäärä muodossa p.k.vvvv" ]
                                                                    ]
                                                                ]
                                                            ]
                                                , div [ class "mt-4" ]
                                                    [ Html.map Update.CalendarMsg (Calendar.view model.calendar) ]
                                                ]
                                            ]
                                        ]

                                Routes.EventsRoute ->
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3" ]
                                            [ Html.map Update.EventListMsg (EventList.view model.eventList model.events.events) ]
                                        ]

                                Routes.MapRoute -> -- Display Map view
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3" ]
                                            [ Html.map Update.MapMsg (Map.view model.map) ]
                                        ]

                                Routes.EventDetail id ->
                                    case List.head (List.filter (\e -> e.id == id) model.events.events) of
                                        Just event ->
                                            div [ class "bg-white shadow rounded-lg" ]
                                                [ div [ class "px-3 py-2 sm:p-3" ]
                                                    [ Html.map Update.EventDetailMsg (EventDetail.view event model.auth) ]
                                                ]

                                        Nothing ->
                                            div [ class "bg-white shadow rounded-lg" ]
                                                [ div [ class "px-3 py-2 sm:p-3" ]
                                                    [ h1 [ class "text-2xl font-bold text-gray-900" ] [ text ("Event not found: " ++ id) ]
                                                    , div [ class "mt-4" ]
                                                        [ a [ href "/", class "text-blue-600 hover:text-blue-800" ] [ text "← Go Home" ] ]
                                                    ]
                                                ]

                                Routes.EditEvent id ->
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3" ]
                                            [ Html.map Update.EventFormMsg (EventForm.view model.eventForm) ]
                                        ]

                                Routes.CreateEvent ->
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3" ]
                                            [ Html.map Update.EventFormMsg (EventForm.view model.eventForm) ]
                                        ]

                                Routes.Callback ->
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3 text-center" ]
                                            [ h1 [ class "text-2xl font-bold text-gray-900 mb-4" ] [ text "Authentication Callback" ]
                                            , p [ class "text-gray-600" ] [ text "Processing authentication..." ]
                                            ]
                                        ]

                                Routes.NotFound ->
                                    div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-3 py-2 sm:p-3 text-center" ]
                                            [ h1 [ class "text-3xl font-bold text-gray-900 mb-4" ] [ text "404" ]
                                            , p [ class "text-xl text-gray-600 mb-6" ] [ text "Page Not Found" ]
                                            , p [ class "text-gray-500 mb-6" ] [ text "The page you are looking for does not exist." ]
                                            , a [ href "/", class "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700" ] [ text "Go Home" ]
                                            ]
                                        ]
                    ]
                ]
            ]
        ]
    }