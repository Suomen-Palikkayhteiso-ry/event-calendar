module Update exposing (Msg(..), update)

import Browser
import Browser.Navigation as Nav
import Calendar
import EventDetail
import EventForm
import EventList
import Events
import Http
import KMLUtils
import Map -- New import
import Model exposing (Model)
import Ports
import Routes
import Task
import Time
import Types
import Url


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CalendarMsg Calendar.Msg
    | EventsMsg Events.Msg
    | EventFormMsg EventForm.Msg
    | EventListMsg EventList.Msg
    | EventDetailMsg EventDetail.Msg
    | MapMsg Map.Msg -- New Message type
    | AuthStored Types.Auth
    | AuthRemoved
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
                    Routes.parseUrl url

                newModel =
                    { model | url = url, route = newRoute }
            in
            case newRoute of
                Routes.CreateEvent ->
                    ( { newModel | eventForm = EventForm.init }, Cmd.none )

                Routes.EditEvent id ->
                    case List.head (List.filter (\e -> e.id == id) model.events.events) of
                        Just event ->
                            let
                                ( updatedEventForm, _ ) =
                                    EventForm.update (EventForm.LoadEvent event) model.eventForm
                            in
                            ( { newModel | eventForm = updatedEventForm }, Cmd.none )

                        Nothing ->
                            ( newModel, Cmd.none )

                Routes.Callback ->
                    case Routes.parseCallbackParams (Maybe.withDefault "" url.query) of
                        Just ( code, state ) ->
                            ( newModel, Ports.handleOAuth2Callback { code = code, state = state } )

                        Nothing ->
                            ( newModel, Cmd.none )

                Routes.MapRoute -> -- Handle MapRoute
                    ( newModel
                    , Map.update Map.InitMap newModel.map |> Tuple.second |> Cmd.map MapMsg
                    )

                _ ->
                    ( newModel, Cmd.none )

        CalendarMsg calendarMsg ->
            let
                ( updatedCalendar, calendarCmd ) =
                    Calendar.update calendarMsg model.calendar
            in
            ( { model | calendar = updatedCalendar }, Cmd.map CalendarMsg calendarCmd )

        EventsMsg eventsMsg ->
            let
                ( updatedEvents, eventsCmd ) =
                    Events.update eventsMsg model.events
            in
            ( { model | events = updatedEvents }, Cmd.map EventsMsg eventsCmd )

        EventFormMsg eventFormMsg ->
            let
                ( updatedEventForm, eventFormCmd ) =
                    EventForm.update eventFormMsg model.eventForm
            in
            case eventFormMsg of
                EventForm.Submit ->
                    case EventForm.getEvent model.eventForm of
                        Just (EventForm.CreateEvent event) ->
                            ( model, Task.perform identity (Task.succeed (EventsMsg (Events.CreateEvent (model.auth |> Maybe.andThen .token) event))) )

                        Just (EventForm.UpdateEvent event) ->
                            ( model, Task.perform identity (Task.succeed (EventsMsg (Events.UpdateEvent (model.auth |> Maybe.andThen .token) event))) )

                        Nothing ->
                            ( { model | eventForm = updatedEventForm }, Cmd.map EventFormMsg eventFormCmd )

                _ ->
                    ( { model | eventForm = updatedEventForm }, Cmd.map EventFormMsg eventFormCmd )

        EventListMsg eventListMsg ->
            case eventListMsg of
                EventList.EditEvent id ->
                    ( model, Nav.pushUrl model.key ("#/events/" ++ id ++ "/edit") )

                EventList.DeleteEvent id ->
                    ( { model | loading = True }, Task.perform identity (Task.succeed (RequestDeleteEvent id)) )

                _ ->
                    let
                        ( updatedEventList, eventListCmd ) =
                            EventList.update eventListMsg model.eventList
                    in
                    ( { model | eventList = updatedEventList }, Cmd.map EventListMsg eventListCmd )

        EventDetailMsg eventDetailMsg ->
            case eventDetailMsg of
                EventDetail.EditEvent id ->
                    ( model, Nav.pushUrl model.key ("#/events/" ++ id ++ "/edit") )

                EventDetail.DeleteEvent id ->
                    ( { model | loading = True }, Task.perform identity (Task.succeed (RequestDeleteEvent id)) )

                _ ->
                    let
                        ( updatedEventDetail, eventDetailCmd ) =
                            EventDetail.update eventDetailMsg model.eventDetail
                    in
                    ( { model | eventDetail = updatedEventDetail }, Cmd.map EventDetailMsg eventDetailCmd )

        MapMsg mapMsg -> -- Handle MapMsg
            let
                ( updatedMap, mapCmd ) =
                    Map.update mapMsg model.map
            in
            ( { model | map = updatedMap }, Cmd.map MapMsg mapCmd )

        AuthStored auth ->
            ( { model | auth = Just auth, error = Nothing }, Cmd.none )

        AuthRemoved ->
            ( { model | auth = Nothing, error = Nothing }, Cmd.none )

        Login ->
            ( { model | loading = True }, Ports.login () )

        Logout ->
            case model.auth of
                Just auth ->
                    ( { model | loading = True }, Http.request
                        { method = "POST"
                        , headers = [ Http.header "Authorization" ("Bearer " ++ auth.token) ]
                        , url = model.pocketbaseUrl ++ "/api/collections/users/auth-refresh"
                        , body = Http.emptyBody
                        , expect = Http.expectWhatever LogoutResult
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Nothing ->
                    ( model, Cmd.none )

        LoginResult result ->
            case result of
                Ok auth ->
                    ( { model | auth = Just auth, loading = False, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | loading = False, error = Just (httpErrorToString error) }, Cmd.none )

        LogoutResult result ->
            case result of
                Ok _ ->
                    ( { model | auth = Nothing, loading = False, error = Nothing }, Ports.logout () )

                Err error ->
                    ( { model | loading = False, error = Just (httpErrorToString error) }, Cmd.none )

        SetDate date ->
            ( { model | selectedDate = date }, Cmd.none )

        SetCurrentTime time ->
            ( { model | calendar = Calendar.update (Calendar.SetCurrentTime time) model.calendar }, Cmd.none )

        RequestDeleteEvent id ->
            case model.auth of
                Just auth ->
                    ( model, Task.perform identity (Task.succeed (EventsMsg (Events.DeleteEvent auth.token id))) )

                Nothing ->
                    ( { model | loading = False }, Cmd.none )

        GoToCreateEvent ->
            ( model, Nav.pushUrl model.key "#/events/create" )

        KMLParsed value ->
            case Decode.decodeValue (Decode.list KMLUtils.eventDecoder) value of
                Ok events ->
                    ( model, Task.perform identity (Task.succeed (EventsMsg (Events.ImportEvents events))) )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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