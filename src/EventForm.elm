module EventForm exposing (..)

import Html exposing (div, text)
import Types exposing (Event, EventState(..), Point)


-- Model


type alias Model =
    { title : String
    , startDate : String
    , endDate : String
    , allDay : Bool
    , location : String
    , description : String
    , url : String
    , image : Maybe String
    , imageDescription : String
    , state : EventState
    , point : Maybe Point
    , errors : List String
    }


init : Model
init =
    { title = ""
    , startDate = ""
    , endDate = ""
    , allDay = False
    , location = ""
    , description = ""
    , url = ""
    , image = Nothing
    , imageDescription = ""
    , state = Draft
    , point = Nothing
    , errors = []
    }


-- Msg


type Msg
    = SetTitle String
    | SetStartDate String
    | SetEndDate String
    | SetAllDay Bool
    | SetLocation String
    | SetDescription String
    | SetUrl String
    | SetImage String
    | SetImageDescription String
    | SetState EventState
    | SetPoint (Maybe Point)
    | Submit
    | Validate


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTitle title ->
            ( { model | title = title }, Cmd.none )

        SetStartDate startDate ->
            ( { model | startDate = startDate }, Cmd.none )

        SetEndDate endDate ->
            ( { model | endDate = endDate }, Cmd.none )

        SetAllDay allDay ->
            ( { model | allDay = allDay }, Cmd.none )

        SetLocation location ->
            ( { model | location = location }, Cmd.none )

        SetDescription description ->
            ( { model | description = description }, Cmd.none )

        SetUrl url ->
            ( { model | url = url }, Cmd.none )

        SetImage image ->
            ( { model | image = Just image }, Cmd.none )

        SetImageDescription imageDescription ->
            ( { model | imageDescription = imageDescription }, Cmd.none )

        SetState state ->
            ( { model | state = state }, Cmd.none )

        SetPoint point ->
            ( { model | point = point }, Cmd.none )

        Submit ->
            let
                newModel =
                    validate model
            in
            if List.isEmpty newModel.errors then
                ( newModel, Cmd.none ) -- TODO: submit the form

            else
                ( newModel, Cmd.none )

        Validate ->
            ( validate model, Cmd.none )


validate : Model -> Model
validate model =
    let
        errors =
            []
                |> addError (String.isEmpty model.title) "Title is required"
                |> addError (String.isEmpty model.startDate) "Start date is required"
    in
    { model | errors = errors }


addError : Bool -> String -> List String -> List String
addError condition error errors =
    if condition then
        error :: errors

    else
        errors


toEvent : Model -> Event
toEvent model =
    { id = "" -- will be set by server
    , title = model.title
    , description = Just model.description
    , startDate = model.startDate
    , endDate = if String.isEmpty model.endDate then Nothing else Just model.endDate
    , allDay = model.allDay
    , url = if String.isEmpty model.url then Nothing else Just model.url
    , location = if String.isEmpty model.location then Nothing else Just model.location
    , state = model.state
    , image = model.image
    , imageDescription = if String.isEmpty model.imageDescription then Nothing else Just model.imageDescription
    , point = model.point
    , created = "" -- server
    , updated = "" -- server
    }


-- View


view : Model -> Html.Html Msg
view model =
    div [] [ text "Event Form" ] -- placeholder