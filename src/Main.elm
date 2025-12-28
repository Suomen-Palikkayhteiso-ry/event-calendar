module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Button
import Calendar
import DateTimePicker
import DateUtils
import EventDetail
import EventForm
import EventList
import Events exposing (Msg(..))
import Html exposing (Html, a, button, div, h1, header, img, label, main_, nav, p, span, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import I18n
import Input
import Json.Decode as Decode
import KMLUtils
import Map
import PocketBase
import Ports
import Task
import Time
import Types
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Url.Parser.Query as Query



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
    , map : Map.Model
    , eventForm : EventForm.Model
    , eventList : EventList.Model
    , auth : Maybe Types.Auth
    , error : Maybe String
    , loading : Bool
    , selectedDate : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( eventsModel, eventsCmd ) =
            Events.update (Events.FetchEvents Nothing) Events.init
    in
    ( Model key url (parseUrl url) Calendar.init eventsModel Map.init EventForm.init EventList.init Nothing Nothing True ""
    , Cmd.batch
        [ Cmd.map EventsMsg eventsCmd
        , Task.perform SetCurrentTime Time.now
        ]
    )



-- ROUTE


type Route
    = Home
    | EventsRoute
    | MapRoute
    | CreateEvent
    | EventDetail String
    | EditEvent String
    | Callback
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map MapRoute (s "map")
        , Parser.map CreateEvent (s "events" </> s "create")
        , Parser.map EventDetail (s "events" </> string)
        , Parser.map EditEvent (s "events" </> string </> s "edit")
        , Parser.map EventsRoute (s "events")
        , Parser.map Callback (s "callback")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    case url.fragment of
        Just fragment ->
            case Parser.parse routeParser { url | path = fragment, fragment = Nothing } of
                Just route ->
                    route

                Nothing ->
                    NotFound

        Nothing ->
            Home


parseCallbackParams : String -> Maybe ( String, Maybe String )
parseCallbackParams query =
    let
        params =
            String.split "&" query
                |> List.map (String.split "=")
                |> List.filterMap
                    (\parts ->
                        case parts of
                            [ key, value ] ->
                                Just ( key, value )

                            _ ->
                                Nothing
                    )

        findParam name =
            params |> List.filter (\( k, _ ) -> k == name) |> List.head |> Maybe.map Tuple.second

        code =
            findParam "code"

        state =
            findParam "state"
    in
    Maybe.map (\c -> ( c, state )) code



-- HELPERS


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CalendarMsg Calendar.Msg
    | EventsMsg Events.Msg
    | MapMsg Map.Msg
    | EventFormMsg EventForm.Msg
    | EventListMsg EventList.Msg
    | EventDetailMsg EventDetail.Msg
    | AuthStored Types.Auth
    | AuthRemoved
    | MapMarkerMoved ( Float, Float )
    | Login
    | Logout
    | LoginResult (Result Http.Error Types.Auth)
    | LogoutResult (Result Http.Error ())
    | SetDate String
    | SetCurrentTime Time.Posix
    | RequestDeleteEvent String
    | GoToCreateEvent
    | KMLParsed Decode.Value
    | NoOp


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
            let
                newRoute =
                    parseUrl url

                newModel =
                    { model | url = url, route = newRoute }
            in
            case newRoute of
                CreateEvent ->
                    ( { newModel | eventForm = EventForm.init }, Cmd.none )

                EditEvent id ->
                    case List.head (List.filter (\e -> e.id == id) model.events.events) of
                        Just event ->
                            let
                                ( updatedEventForm, _ ) =
                                    EventForm.update (EventForm.LoadEvent event) model.eventForm
                            in
                            ( { newModel | eventForm = updatedEventForm }, Cmd.none )

                        Nothing ->
                            ( newModel, Cmd.none )

                Callback ->
                    case parseCallbackParams (Maybe.withDefault "" url.query) of
                        Just ( code, state ) ->
                            ( newModel, Ports.handleOAuth2Callback { code = code, state = state } )

                        Nothing ->
                            ( newModel, Cmd.none )

                MapRoute ->
                    ( newModel, Task.perform identity (Task.succeed (MapMsg Map.InitMap)) )

                _ ->
                    ( newModel, Cmd.none )

        CalendarMsg calendarMsg ->
            ( { model | calendar = Calendar.update calendarMsg model.calendar }
            , Cmd.none
            )

        SetCurrentTime time ->
            ( { model | calendar = Calendar.update (Calendar.SetDate time) model.calendar }
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

                        Events.EventCreated (Ok event) ->
                            Calendar.update (Calendar.AddEvent event) model.calendar

                        Events.EventUpdated (Ok event) ->
                            Calendar.update (Calendar.UpdateEvent event) model.calendar

                        _ ->
                            model.calendar

                updatedMap =
                    case eventsMsg of
                        Events.EventsFetched (Ok events) ->
                            Tuple.first (Map.update (Map.SetEvents events) model.map)

                        Events.EventCreated (Ok event) ->
                            Tuple.first (Map.update (Map.SetEvents (event :: model.events.events)) model.map)

                        Events.EventUpdated (Ok event) ->
                            let
                                updatedEventsList =
                                    List.map
                                        (\e ->
                                            if e.id == event.id then
                                                event

                                            else
                                                e
                                        )
                                        model.events.events
                            in
                            Tuple.first (Map.update (Map.SetEvents updatedEventsList) model.map)

                        _ ->
                            model.map

                newLoading =
                    case eventsMsg of
                        Events.EventsFetched _ ->
                            False

                        _ ->
                            model.loading

                newError =
                    updatedEvents.error

                updatedEventForm =
                    case eventsMsg of
                        Events.EventCreated _ ->
                            Tuple.first (EventForm.update (EventForm.SetLoading False) model.eventForm)

                        Events.EventUpdated _ ->
                            Tuple.first (EventForm.update (EventForm.SetLoading False) model.eventForm)

                        _ ->
                            model.eventForm

                redirectCmd =
                    case eventsMsg of
                        Events.EventDeleted _ (Ok ()) ->
                            Nav.pushUrl model.key "#/"

                        Events.EventCreated (Ok _) ->
                            Nav.pushUrl model.key "#/"

                        Events.EventUpdated (Ok _) ->
                            Nav.pushUrl model.key "#/"

                        _ ->
                            Cmd.none
            in
            ( { model | events = updatedEvents, calendar = updatedCalendar, map = updatedMap, loading = newLoading, error = newError, eventForm = updatedEventForm }
            , Cmd.batch [ Cmd.map EventsMsg eventsCmd, redirectCmd ]
            )

        MapMsg mapMsg ->
            let
                ( updatedMap, mapCmd ) =
                    Map.update mapMsg model.map
            in
            ( { model | map = updatedMap }
            , Cmd.map MapMsg mapCmd
            )

        EventFormMsg eventFormMsg ->
            let
                ( updatedEventForm, action ) =
                    EventForm.update eventFormMsg model.eventForm

                cmd =
                    case action of
                        Just (EventForm.CreateEvent event) ->
                            Task.perform identity (Task.succeed (EventsMsg (Events.CreateEvent (model.auth |> Maybe.andThen .token) event)))

                        Just (EventForm.UpdateEvent id event) ->
                            Task.perform identity (Task.succeed (EventsMsg (Events.UpdateEvent (model.auth |> Maybe.andThen .token) id event)))

                        Nothing ->
                            Cmd.none
            in
            ( { model | eventForm = updatedEventForm }
            , cmd
            )

        EventListMsg eventListMsg ->
            case eventListMsg of
                EventList.DeleteEvent id ->
                    ( model
                    , Task.perform identity (Task.succeed (EventsMsg (Events.DeleteEvent (model.auth |> Maybe.andThen .token) id)))
                    )

                EventList.EditEvent id ->
                    ( model, Nav.pushUrl model.key ("#/events/" ++ id ++ "/edit") )

                EventList.FileLoaded content ->
                    ( model, Ports.parseKMLContent content )

                _ ->
                    let
                        ( updatedEventList, cmd ) =
                            EventList.update eventListMsg model.eventList
                    in
                    ( { model | eventList = updatedEventList }, Cmd.map EventListMsg cmd )

        EventDetailMsg eventDetailMsg ->
            case eventDetailMsg of
                EventDetail.EditEvent id ->
                    ( model, Nav.pushUrl model.key ("#/events/" ++ id ++ "/edit") )

                EventDetail.DeleteEvent id ->
                    ( model
                    , Task.perform identity (Task.succeed (EventsMsg (Events.DeleteEvent (model.auth |> Maybe.andThen .token) id)))
                    )

                EventDetail.Back ->
                    ( model, Nav.pushUrl model.key "#/events" )

        KMLParsed value ->
            case Decode.decodeValue (Decode.list KMLUtils.rawKMLDecoder) value of
                Ok rawList ->
                    let
                        events =
                            List.filterMap KMLUtils.processRawKML rawList

                        cmds =
                            List.map (\e -> Task.perform identity (Task.succeed (EventsMsg (Events.CreateEvent (model.auth |> Maybe.andThen .token) e)))) events
                    in
                    ( model, Cmd.batch cmds )

                Err _ ->
                    ( model, Cmd.none )

        -- Handle error?
        AuthStored auth ->
            ( { model | auth = Just auth }, Cmd.none )

        AuthRemoved ->
            ( { model | auth = Nothing }, Cmd.none )

        MapMarkerMoved pos ->
            let
                ( updatedMap, _ ) =
                    Map.update (Map.MarkerMoved pos) model.map
            in
            ( { model | map = updatedMap }
            , Cmd.none
            )

        Login ->
            ( model, PocketBase.initiateOAuth2Login "oidc" )

        Logout ->
            case model.auth of
                Just auth ->
                    case auth.token of
                        Just token ->
                            ( model, PocketBase.logout token LogoutResult )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        LoginResult result ->
            case result of
                Ok auth ->
                    ( { model | auth = Just auth, error = Nothing, loading = False }, Ports.storeAuth auth )

                Err err ->
                    ( { model | error = Just (httpErrorToString err), loading = False }, Cmd.none )

        LogoutResult result ->
            case result of
                Ok _ ->
                    ( { model | auth = Nothing, error = Nothing, loading = False }, Ports.removeAuth () )

                Err err ->
                    ( { model | error = Just (httpErrorToString err), loading = False }, Cmd.none )

        SetDate dateStr ->
            let
                -- Parse date string YYYY-MM-DD
                posix =
                    DateUtils.parseUTCDate (dateStr ++ "T00:00:00Z")
            in
            ( { model | selectedDate = dateStr, calendar = Calendar.update (Calendar.SetDate posix) model.calendar }
            , Cmd.none
            )

        RequestDeleteEvent id ->
            ( model
            , Task.perform identity (Task.succeed (EventsMsg (Events.DeleteEvent (model.auth |> Maybe.andThen .token) id)))
            )

        GoToCreateEvent ->
            ( model, Nav.pushUrl model.key "#/events/create" )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authStored
            (\value ->
                case Decode.decodeValue Types.authDecoder value of
                    Ok auth ->
                        AuthStored auth

                    Err _ ->
                        NoOp
            )
        , Ports.authRemoved (always AuthRemoved)
        , Ports.mapMarkerMoved MapMarkerMoved
        , Ports.kmlContentParsed KMLParsed
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = I18n.get "event_calendar"
    , body =
        [ div [ class "min-h-screen flex flex-col" ]
            [ header [ class "bg-white shadow-sm border-b border-gray-200 sticky top-0 z-50" ]
                [ div [ class "max-w-7xl mx-auto px-4 sm:px-6 lg:px-8" ]
                    [ div [ class "flex justify-between items-center h-12" ]
                        [ nav [ class "flex space-x-8" ]
                            [ a [ href "#/", class "text-gray-900 hover:text-blue-600 px-3 py-2 text-sm font-medium" ] [ text (I18n.get "home") ]
                            , a [ href "#/map", class "text-gray-500 hover:text-blue-600 px-3 py-2 text-sm font-medium" ] [ text (I18n.get "map") ]
                            , if model.auth /= Nothing then
                                a [ href "#/events", class "text-gray-500 hover:text-blue-600 px-3 py-2 text-sm font-medium" ] [ text (I18n.get "events") ]

                              else
                                text ""
                            ]
                        , div [ class "flex items-center space-x-4" ]
                            [ case model.auth of
                                Just auth ->
                                    div [ class "flex items-center space-x-4" ]
                                        [ span [ class "text-sm text-gray-700" ]
                                            [ text
                                                ("Logged in as: "
                                                    ++ (case auth.user of
                                                            Just user ->
                                                                case user.username of
                                                                    Just username ->
                                                                        username

                                                                    Nothing ->
                                                                        user.email

                                                            Nothing ->
                                                                "Unknown"
                                                       )
                                                )
                                            ]
                                        , Button.view
                                            { variant = Button.Secondary
                                            , size = Button.Sm
                                            , disabled = False
                                            , type_ = "button"
                                            , ariaLabel = Nothing
                                            , onClick = Just Logout
                                            , children = [ text (I18n.get "logout") ]
                                            }
                                        ]

                                Nothing ->
                                    Button.view
                                        { variant = Button.Primary
                                        , size = Button.Sm
                                        , disabled = False
                                        , type_ = "button"
                                        , ariaLabel = Nothing
                                        , onClick = Just Login
                                        , children = [ text (I18n.get "login") ]
                                        }
                            ]
                        ]
                    ]
                , case model.error of
                    Just err ->
                        div [ class "bg-red-50 border-l-4 border-red-400 p-4 max-w-7xl mx-auto" ]
                            [ div [ class "flex" ]
                                [ div [ class "ml-3" ]
                                    [ p [ class "text-sm text-red-700" ] [ text err ]
                                    ]
                                ]
                            ]

                    Nothing ->
                        text ""
                ]
            , main_ [ class "flex-1 bg-gray-50" ]
                [ div [ class "max-w-7xl mx-auto py-4 px-4 sm:px-6 lg:px-8" ]
                    [ if model.loading then
                        div [ class "flex justify-center items-center h-64" ]
                            [ div [ class "animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600" ] []
                            ]

                      else
                        case model.route of
                            Home ->
                                div [ class "space-y-6" ]
                                    [ div [ class "bg-white shadow rounded-lg" ]
                                        [ div [ class "px-4 py-3 sm:p-4" ]
                                            [ h1 [ class "text-2xl font-bold text-gray-900 mb-4" ] [ text (I18n.get "public_calendar") ]
                                            , case model.auth of
                                                Just _ ->
                                                    div [ class "mb-6 flex items-center justify-between" ]
                                                        [ div [ class "flex items-center space-x-4" ]
                                                            [ label [ class "block text-sm font-medium text-gray-700" ] [ text (I18n.get "select_date") ]
                                                            , Input.view
                                                                { type_ = "date"
                                                                , value = model.selectedDate
                                                                , placeholder = Nothing
                                                                , required = False
                                                                , disabled = False
                                                                , readonly = False
                                                                , ariaLabel = Just "Select Date"
                                                                , ariaDescribedBy = Nothing
                                                                , ariaInvalid = False
                                                                , ariaRequired = False
                                                                , id = Just "calendar-date"
                                                                , name = Nothing
                                                                , pattern = Nothing
                                                                , min = Nothing
                                                                , max = Nothing
                                                                , step = Nothing
                                                                , accept = Nothing
                                                                , autofocus = False
                                                                , class = Nothing
                                                                , onInput = Just SetDate
                                                                , onChange = Nothing
                                                                , onBlur = Nothing
                                                                , onFocus = Nothing
                                                                }
                                                            ]
                                                        , button [ class "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500", onClick GoToCreateEvent, title "Add new event" ]
                                                            [ text ("+ " ++ I18n.get "add_new_event") ]
                                                        ]

                                                Nothing ->
                                                    div [ class "mb-6 bg-blue-50 border border-blue-200 rounded-md p-4" ]
                                                        [ p [ class "text-sm text-blue-800 mb-4" ]
                                                            [ text (I18n.get "non_member_prefix")
                                                            , a [ href "mailto:suomenpalikkayhteisory@outlook.com", class "text-blue-600 hover:text-blue-800 underline" ] [ text (I18n.get "send_event_email") ]
                                                            , text "."
                                                            ]
                                                        , div []
                                                            [ label [ class "block text-sm font-medium text-gray-700 mb-2" ] [ text (I18n.get "select_date") ]
                                                            , Input.view
                                                                { type_ = "date"
                                                                , value = model.selectedDate
                                                                , placeholder = Nothing
                                                                , required = False
                                                                , disabled = False
                                                                , readonly = False
                                                                , ariaLabel = Just "Select Date"
                                                                , ariaDescribedBy = Nothing
                                                                , ariaInvalid = False
                                                                , ariaRequired = False
                                                                , id = Just "calendar-date"
                                                                , name = Nothing
                                                                , pattern = Nothing
                                                                , min = Nothing
                                                                , max = Nothing
                                                                , step = Nothing
                                                                , accept = Nothing
                                                                , autofocus = False
                                                                , class = Nothing
                                                                , onInput = Just SetDate
                                                                , onChange = Nothing
                                                                , onBlur = Nothing
                                                                , onFocus = Nothing
                                                                }
                                                            ]
                                                        ]
                                            , div [ class "mt-6" ]
                                                [ Html.map CalendarMsg (Calendar.view model.calendar) ]
                                            ]
                                        ]
                                    ]

                            MapRoute ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4" ]
                                        [ Html.map MapMsg (Map.view model.map) ]
                                    ]

                            EventsRoute ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4" ]
                                        [ Html.map EventListMsg (EventList.view model.eventList model.events.events) ]
                                    ]

                            EventDetail id ->
                                case List.head (List.filter (\e -> e.id == id) model.events.events) of
                                    Just event ->
                                        div [ class "bg-white shadow rounded-lg" ]
                                            [ div [ class "px-4 py-3 sm:p-4" ]
                                                [ Html.map EventDetailMsg (EventDetail.view event model.auth) ]
                                            ]

                                    Nothing ->
                                        div [ class "bg-white shadow rounded-lg" ]
                                            [ div [ class "px-4 py-3 sm:p-4" ]
                                                [ h1 [ class "text-2xl font-bold text-gray-900" ] [ text ("Event not found: " ++ id) ]
                                                , div [ class "mt-4" ]
                                                    [ a [ href "/", class "text-blue-600 hover:text-blue-800" ] [ text "← Go Home" ] ]
                                                ]
                                            ]

                            EditEvent id ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4" ]
                                        [ Html.map EventFormMsg (EventForm.view model.eventForm) ]
                                    ]

                            CreateEvent ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4" ]
                                        [ Html.map EventFormMsg (EventForm.view model.eventForm) ]
                                    ]

                            Callback ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4" ]
                                        [ h1 [ class "text-2xl font-bold text-gray-900 mb-4" ] [ text "Authentication Callback" ]
                                        , p [ class "text-gray-600" ] [ text "Processing authentication..." ]
                                        ]
                                    ]

                            NotFound ->
                                div [ class "bg-white shadow rounded-lg" ]
                                    [ div [ class "px-4 py-3 sm:p-4 text-center" ]
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
