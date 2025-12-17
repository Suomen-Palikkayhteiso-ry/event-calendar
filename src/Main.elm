module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Button
import Calendar
import EventForm
import Events
import Html exposing (Html, a, div, h1, header, main_, nav, p, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Input
import Map
import PocketBase
import Ports
import Time
import Types
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
    , map : Map.Model
    , eventForm : EventForm.Model
    , auth : Maybe Types.Auth
    , loginEmail : String
    , loginPassword : String
    , error : Maybe String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( eventsModel, eventsCmd ) =
            Events.update Events.FetchEvents Events.init
    in
    ( Model key url (parseUrl url) Calendar.init eventsModel Map.init EventForm.init Nothing "" "" Nothing
    , Cmd.map EventsMsg eventsCmd
    )



-- ROUTE


type Route
    = Home
    | MapRoute
    | EventDetail String
    | EditEvent String
    | Callback
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map MapRoute (s "map")
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
    | AuthStored Types.Auth
    | AuthRemoved
    | MapMarkerMoved ( Float, Float )
    | Login
    | Logout
    | LoginResult (Result Http.Error Types.Auth)
    | LogoutResult (Result Http.Error ())
    | UpdateLoginEmail String
    | UpdateLoginPassword String


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
                ( updatedEventForm, eventFormCmd ) =
                    EventForm.update eventFormMsg model.eventForm
            in
            ( { model | eventForm = updatedEventForm }
            , Cmd.map EventFormMsg eventFormCmd
            )

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
            let
                credentials =
                    { email = model.loginEmail, password = model.loginPassword }
            in
            ( model, PocketBase.login credentials LoginResult )

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
                    ( { model | auth = Just auth, error = Nothing }, Ports.storeAuth auth )

                Err err ->
                    ( { model | error = Just (httpErrorToString err) }, Cmd.none )

        LogoutResult result ->
            case result of
                Ok _ ->
                    ( { model | auth = Nothing, error = Nothing }, Ports.removeAuth () )

                Err err ->
                    ( { model | error = Just (httpErrorToString err) }, Cmd.none )

        UpdateLoginEmail email ->
            ( { model | loginEmail = email }, Cmd.none )

        UpdateLoginPassword password ->
            ( { model | loginPassword = password }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.authStored AuthStored
        , Ports.authRemoved (always AuthRemoved)
        , Ports.mapMarkerMoved MapMarkerMoved
        ]



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
                    , a [ href "/map" ] [ text "Map" ]
                    , text " | "
                    , a [ href "/events/123" ] [ text "Event Detail" ]
                    , text " | "
                    , a [ href "/events/123/edit" ] [ text "Edit Event" ]
                    , text " | "
                    , a [ href "/callback" ] [ text "Callback" ]
                    ]
                , div []
                    [ case model.auth of
                        Just auth ->
                            div []
                                [ text ("Logged in as: " ++ (Maybe.withDefault "Unknown" (Maybe.map .email auth.user)))
                                , Button.view
                                    { variant = Button.Secondary
                                    , size = Button.Md
                                    , disabled = False
                                    , type_ = "button"
                                    , ariaLabel = Nothing
                                    , onClick = Just Logout
                                    , children = [ text "Logout" ]
                                    }
                                ]

                        Nothing ->
                            div []
                                [ Input.view
                                    { type_ = "email"
                                    , value = model.loginEmail
                                    , placeholder = Just "Email"
                                    , required = False
                                    , disabled = False
                                    , readonly = False
                                    , ariaLabel = Nothing
                                    , ariaDescribedBy = Nothing
                                    , ariaInvalid = False
                                    , ariaRequired = False
                                    , id = Nothing
                                    , name = Nothing
                                    , pattern = Nothing
                                    , min = Nothing
                                    , max = Nothing
                                    , step = Nothing
                                    , accept = Nothing
                                    , autofocus = False
                                    , class = Nothing
                                    , onInput = Just UpdateLoginEmail
                                    , onChange = Nothing
                                    , onBlur = Nothing
                                    , onFocus = Nothing
                                    }
                                , Input.view
                                    { type_ = "password"
                                    , value = model.loginPassword
                                    , placeholder = Just "Password"
                                    , required = False
                                    , disabled = False
                                    , readonly = False
                                    , ariaLabel = Nothing
                                    , ariaDescribedBy = Nothing
                                    , ariaInvalid = False
                                    , ariaRequired = False
                                    , id = Nothing
                                    , name = Nothing
                                    , pattern = Nothing
                                    , min = Nothing
                                    , max = Nothing
                                    , step = Nothing
                                    , accept = Nothing
                                    , autofocus = False
                                    , class = Nothing
                                    , onInput = Just UpdateLoginPassword
                                    , onChange = Nothing
                                    , onBlur = Nothing
                                    , onFocus = Nothing
                                    }
                                , Button.view
                                    { variant = Button.Primary
                                    , size = Button.Md
                                    , disabled = False
                                    , type_ = "button"
                                    , ariaLabel = Nothing
                                    , onClick = Just Login
                                    , children = [ text "Login" ]
                                    }
                                ]
                    ]
                , case model.error of
                    Just err ->
                        div [ style "color" "red" ] [ text err ]

                    Nothing ->
                        text ""
                ]
            , main_ []
                [ case model.route of
                    Home ->
                        Html.map CalendarMsg (Calendar.view model.calendar)

                    MapRoute ->
                        Html.map MapMsg (Map.view model.map)

                    EventDetail id ->
                        case List.head (List.filter (\e -> e.id == id) model.events.events) of
                            Just event ->
                                div []
                                    [ h1 [] [ text event.title ]
                                    , p [] [ text (Maybe.withDefault "" event.description) ]
                                    , p [] [ text ("Start: " ++ event.startDate) ]
                                    , p [] [ text ("Location: " ++ Maybe.withDefault "" event.location) ]
                                    , a [ href ("/events/" ++ id ++ "/edit") ] [ text "Edit" ]
                                    ]

                            Nothing ->
                                h1 [] [ text ("Event not found: " ++ id) ]

                    EditEvent id ->
                        Html.map EventFormMsg (EventForm.view model.eventForm)

                    Callback ->
                        div []
                            [ h1 [] [ text "Authentication Callback" ]
                            , p [] [ text "Processing authentication..." ]
                            ]

                    NotFound ->
                        div []
                            [ h1 [] [ text "404 - Page Not Found" ]
                            , p [] [ text "The page you are looking for does not exist." ]
                            , a [ href "/" ] [ text "Go Home" ]
                            ]
                ]
            ]
        ]
    }
