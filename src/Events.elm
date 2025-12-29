module Events exposing (..)

import Http
import PocketBase
import Types exposing (Event)



-- Model


type alias Model =
    { events : List Event
    , loading : Bool
    , error : Maybe String
    , pocketbaseUrl : String
    }


init : String -> Model
init pocketbaseUrl =
    { events = []
    , loading = False
    , error = Nothing
    , pocketbaseUrl = pocketbaseUrl
    }



-- Msg


type Msg
    = FetchEvents (Maybe String)
    | EventsFetched (Result Http.Error (List Event))
    | FetchEvent (Maybe String) String
    | EventFetched (Result Http.Error Event)
    | CreateEvent (Maybe String) Event
    | EventCreated (Result Http.Error Event)
    | UpdateEvent (Maybe String) String Event
    | EventUpdated (Result Http.Error Event)
    | DeleteEvent (Maybe String) String
    | EventDeleted String (Result Http.Error ())
    | MarkDeleted (Maybe String) String
    | EventMarkedDeleted (Result Http.Error Event)



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchEvents token ->
            ( { model | loading = True, error = Nothing }
            , PocketBase.getEvents model.pocketbaseUrl token EventsFetched
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

        CreateEvent token event ->
            ( model
            , PocketBase.createEvent model.pocketbaseUrl token event EventCreated
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

        UpdateEvent token id updatedEvent ->
            ( model
            , PocketBase.updateEvent model.pocketbaseUrl token id updatedEvent EventUpdated
            )

        EventUpdated result ->
            case result of
                Ok updatedEvent ->
                    ( { model
                        | events =
                            List.map
                                (\e ->
                                    if e.id == updatedEvent.id then
                                        updatedEvent

                                    else
                                        e
                                )
                                model.events
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to update event" }
                    , Cmd.none
                    )

        DeleteEvent token id ->
            ( model
            , PocketBase.deleteEvent model.pocketbaseUrl token id (EventDeleted id)
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

        FetchEvent token id ->
            ( model
            , PocketBase.getEvent model.pocketbaseUrl token id EventFetched
            )

        EventFetched result ->
            case result of
                Ok event ->
                    ( model
                    , Cmd.none
                      -- Will be followed by update
                    )

                Err _ ->
                    ( { model | error = Just "Failed to fetch event" }
                    , Cmd.none
                    )

        MarkDeleted token id ->
            ( model
            , PocketBase.getEvent model.pocketbaseUrl
                token
                id
                (\result ->
                    case result of
                        Ok event ->
                            UpdateEvent token id { event | state = Types.Deleted }

                        Err err ->
                            EventMarkedDeleted (Err err)
                )
            )

        EventMarkedDeleted result ->
            case result of
                Ok event ->
                    ( { model
                        | events =
                            List.map
                                (\e ->
                                    if e.id == event.id then
                                        event

                                    else
                                        e
                                )
                                model.events
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "Failed to mark event as deleted" }
                    , Cmd.none
                    )
