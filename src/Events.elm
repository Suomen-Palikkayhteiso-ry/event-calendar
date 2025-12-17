module Events exposing (..)

import Http
import PocketBase
import Types exposing (Event)


-- Model


type alias Model =
    { events : List Event
    , loading : Bool
    , error : Maybe String
    }


init : Model
init =
    { events = []
    , loading = False
    , error = Nothing
    }


-- Msg


type Msg
    = FetchEvents
    | EventsFetched (Result Http.Error (List Event))
    | CreateEvent Event
    | EventCreated (Result Http.Error Event)
    | UpdateEvent String Event
    | EventUpdated (Result Http.Error Event)
    | DeleteEvent String
    | EventDeleted String (Result Http.Error ())


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchEvents ->
            ( { model | loading = True, error = Nothing }
            , PocketBase.getEvents Nothing EventsFetched
            )

        EventsFetched result ->
            case result of
                Ok events ->
                    ( { model | events = events, loading = False }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | loading = False, error = Just "Failed to fetch events" }
                    , Cmd.none
                    )

        CreateEvent event ->
            ( model
            , PocketBase.createEvent Nothing event EventCreated
            )

        EventCreated result ->
            case result of
                Ok newEvent ->
                    ( { model | events = newEvent :: model.events }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to create event" }
                    , Cmd.none
                    )

        UpdateEvent id updatedEvent ->
            ( model
            , PocketBase.updateEvent Nothing id updatedEvent EventUpdated
            )

        EventUpdated result ->
            case result of
                Ok updatedEvent ->
                    ( { model | events = List.map (\e -> if e.id == updatedEvent.id then updatedEvent else e) model.events }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to update event" }
                    , Cmd.none
                    )

        DeleteEvent id ->
            ( model
            , PocketBase.deleteEvent Nothing id (EventDeleted id)
            )

        EventDeleted id result ->
            case result of
                Ok () ->
                    ( { model | events = List.filter (\e -> e.id /= id) model.events }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to delete event" }
                    , Cmd.none
                    )