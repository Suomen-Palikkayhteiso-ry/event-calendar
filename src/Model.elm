module Model exposing (Model, init)

import Browser.Navigation as Nav
import Calendar
import EventForm
import EventList
import Events
import Routes
import Time
import Map -- New import
import Types
import Url


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Routes.Route
    , calendar : Calendar.Model
    , events : Events.Model
    , eventForm : EventForm.Model
    , eventList : EventList.Model
    , map : Map.Model -- New field
    , auth : Maybe Types.Auth
    , error : Maybe String
    , loading : Bool
    , selectedDate : String
    , pocketbaseUrl : String
    }


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init maybePocketbaseUrl url key =
    let
        pocketbaseUrl =
            Maybe.withDefault "https://data.suomenpalikkayhteiso.fi/api" maybePocketbaseUrl

        ( eventsModel, eventsCmd ) =
            Events.update (Events.FetchEvents Nothing) (Events.init pocketbaseUrl)
    in
    ( Model key url (Routes.parseUrl url) Calendar.init eventsModel EventForm.init EventList.init Map.init Nothing Nothing True "" pocketbaseUrl -- Initialize Map.init
    , eventsCmd
    )