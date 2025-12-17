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
    = FetchEvents (Maybe String)
    | EventsFetched (Result Http.Error (List Event))
    | CreateEvent (Maybe String) Event
    | EventCreated (Result Http.Error Event)
    | UpdateEvent (Maybe String) String Event
    | EventUpdated (Result Http.Error Event)
    | DeleteEvent (Maybe String) String
    | EventDeleted String (Result Http.Error ())



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchEvents token ->
            ( { model | loading = True, error = Nothing }
            , PocketBase.getEvents token EventsFetched
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
            , PocketBase.createEvent token event EventCreated
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
            , PocketBase.updateEvent token id updatedEvent EventUpdated
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
            , PocketBase.deleteEvent token id (EventDeleted id)
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
