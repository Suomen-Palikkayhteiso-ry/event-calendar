module Calendar exposing (..)module Calendar exposing (..)

import Time
import Types exposing (CalendarState, Event)


-- Model


type alias Model =
    CalendarState


init : Model
init =
    { view = "month"
    , events = []
    , currentDate = Time.millisToPosix 0 -- TODO: set to current date
    , locale = "en"
    , firstDay = 1
    }


-- Msg


type Msg
    = SetView String
    | SetDate Time.Posix
    | SetEvents (List Event)
    | Next
    | Previous


-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetView view ->
            { model | view = view }

        SetDate date ->
            { model | currentDate = date }

        SetEvents events ->
            { model | events = events }

        Next ->
            -- TODO: calculate next date based on view
            model

        Previous ->
            -- TODO: calculate previous date based on view
            model