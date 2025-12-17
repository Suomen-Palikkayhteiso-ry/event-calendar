module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Calendar
import Events
import Html exposing (Html, a, div, h1, header, main_, nav, text)
import Html.Attributes exposing (..)
import Time
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , calendar : Calendar.Model
    , events : Events.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( eventsModel, eventsCmd ) = Events.update Events.FetchEvents Events.init
    in
    ( Model key url (parseUrl url) Calendar.init eventsModel
    , Cmd.map EventsMsg eventsCmd
    )


-- ROUTE


type Route
    = Home
    | EventDetail String
    | EditEvent String
    | Callback
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map EventDetail (s "events" </> string)
        , Parser.map EditEvent (s "events" </> string </> s "edit")
        , Parser.map Callback (s "callback")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    case Parser.parse routeParser url of
        Just route ->
            route

        Nothing ->
            NotFound


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CalendarMsg Calendar.Msg
    | EventsMsg Events.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url, route = parseUrl url }
            , Cmd.none
            )

        CalendarMsg calendarMsg ->
            ( { model | calendar = Calendar.update calendarMsg model.calendar }
            , Cmd.none
            )

        EventsMsg eventsMsg ->
            let
                ( updatedEvents, eventsCmd ) =
                    Events.update eventsMsg model.events

                updatedCalendar =
                    case eventsMsg of
                        Events.EventsFetched (Ok events) ->
                            Calendar.update (Calendar.SetEvents events) model.calendar

                        _ ->
                            model.calendar
            in
            ( { model | events = updatedEvents, calendar = updatedCalendar }
            , Cmd.map EventsMsg eventsCmd
            )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Event Calendar"
    , body =
        [ div []
            [ header []
                [ nav []
                    [ a [ href "/" ] [ text "Home" ]
                    , text " | "
                    , a [ href "/events/123" ] [ text "Event Detail" ]
                    , text " | "
                    , a [ href "/events/123/edit" ] [ text "Edit Event" ]
                    , text " | "
                    , a [ href "/callback" ] [ text "Callback" ]
                    ]
                ]
            , main_ []
                [ case model.route of
                    Home ->
                        Html.map CalendarMsg (Calendar.view model.calendar)

                    EventDetail id ->
                        h1 [] [ text ("Event Detail Page for " ++ id) ]

                    EditEvent id ->
                        h1 [] [ text ("Edit Event Page for " ++ id) ]

                    Callback ->
                        h1 [] [ text "Auth Callback Page" ]

                    NotFound ->
                        h1 [] [ text "Not Found" ]
                ]
            ]
        ]
    }