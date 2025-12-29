module EventForm exposing (..)

import DateTimePicker
import Html exposing (button, div, input, label, li, text, textarea, ul)
import Html.Attributes exposing (checked, disabled, for, id, name, placeholder, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Types exposing (Event, EventState(..), Point)


type Action
    = CreateEvent Event
    | UpdateEvent String Event



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
    , mode : Mode
    , loading : Bool
    }


type Mode
    = Create
    | Edit String


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
    , mode = Create
    , loading = False
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
    | LoadEvent Event
    | SetLoading Bool



-- Update


update : Msg -> Model -> ( Model, Maybe Action )
update msg model =
    case msg of
        SetTitle title ->
            ( { model | title = title }, Nothing )

        SetStartDate startDate ->
            ( { model | startDate = startDate }, Nothing )

        SetEndDate endDate ->
            ( { model | endDate = endDate }, Nothing )

        SetAllDay allDay ->
            ( { model | allDay = allDay }, Nothing )

        SetLocation location ->
            ( { model | location = location }, Nothing )

        SetDescription description ->
            ( { model | description = description }, Nothing )

        SetUrl url ->
            ( { model | url = url }, Nothing )

        SetImage image ->
            ( { model | image = Just image }, Nothing )

        SetImageDescription imageDescription ->
            ( { model | imageDescription = imageDescription }, Nothing )

        SetState state ->
            ( { model | state = state }, Nothing )

        SetPoint point ->
            ( { model | point = point }, Nothing )

        Submit ->
            let
                newModel =
                    validate model
            in
            if List.isEmpty newModel.errors then
                case model.mode of
                    Create ->
                        ( { newModel | loading = True }, Just (CreateEvent (toEvent newModel)) )

                    Edit id ->
                        ( { newModel | loading = True }, Just (UpdateEvent id (toEvent newModel)) )

            else
                ( newModel, Nothing )

        Validate ->
            ( validate model, Nothing )

        LoadEvent event ->
            ( fromEvent event, Nothing )

        SetLoading loading ->
            ( { model | loading = loading }, Nothing )


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
    , endDate =
        if String.isEmpty model.endDate then
            Nothing

        else
            Just model.endDate
    , allDay = model.allDay
    , url =
        if String.isEmpty model.url then
            Nothing

        else
            Just model.url
    , location =
        if String.isEmpty model.location then
            Nothing

        else
            Just model.location
    , state = model.state
    , image = model.image
    , imageDescription =
        if String.isEmpty model.imageDescription then
            Nothing

        else
            Just model.imageDescription
    , point = model.point
    , created = "" -- server
    , updated = "" -- server
    }


fromEvent : Event -> Model
fromEvent event =
    { title = event.title
    , startDate = event.startDate
    , endDate = Maybe.withDefault "" event.endDate
    , allDay = event.allDay
    , location = Maybe.withDefault "" event.location
    , description = Maybe.withDefault "" event.description
    , url = Maybe.withDefault "" event.url
    , image = event.image
    , imageDescription = Maybe.withDefault "" event.imageDescription
    , state = event.state
    , point = event.point
    , errors = []
    , mode = Edit event.id
    , loading = False
    }



-- View


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ label [ for "title" ] [ text "Title" ]
            , input [ type_ "text", id "title", value model.title, onInput SetTitle, placeholder "Event title" ] []
            ]
        , DateTimePicker.view
            { value = model.startDate
            , label = "Start Date"
            , id = "startDate"
            , disabled = False
            , allDay = model.allDay
            , onChange = SetStartDate
            }
        , DateTimePicker.view
            { value = model.endDate
            , label = "End Date"
            , id = "endDate"
            , disabled = False
            , allDay = model.allDay
            , onChange = SetEndDate
            }
        , div []
            [ label [ for "allDay" ] [ text "All Day" ]
            , input [ type_ "checkbox", id "allDay", checked model.allDay, onCheck SetAllDay ] []
            ]
        , div []
            [ label [ for "location" ] [ text "Location" ]
            , input [ type_ "text", id "location", value model.location, onInput SetLocation, placeholder "Event location" ] []
            ]
        , div []
            [ label [ for "description" ] [ text "Description" ]
            , textarea [ id "description", value model.description, onInput SetDescription, placeholder "Event description" ] []
            ]
        , div []
            [ label [ for "url" ] [ text "URL" ]
            , input [ type_ "url", id "url", value model.url, onInput SetUrl, placeholder "Event URL" ] []
            ]
        , div []
            [ label [ for "image" ] [ text "Image URL" ]
            , input [ type_ "url", id "image", value (Maybe.withDefault "" model.image), onInput SetImage, placeholder "Image URL" ] []
            ]
        , div []
            [ label [ for "imageDescription" ] [ text "Image Description" ]
            , input [ type_ "text", id "imageDescription", value model.imageDescription, onInput SetImageDescription, placeholder "Image description" ] []
            ]
        , div []
            [ label [] [ text "State" ]
            , div []
                [ label [] [ input [ type_ "radio", name "state", checked (model.state == Draft), onClick (SetState Draft) ] [], text "Draft" ]
                , label [] [ input [ type_ "radio", name "state", checked (model.state == Pending), onClick (SetState Pending) ] [], text "Pending" ]
                , label [] [ input [ type_ "radio", name "state", checked (model.state == Published), onClick (SetState Published) ] [], text "Published" ]
                , label [] [ input [ type_ "radio", name "state", checked (model.state == Deleted), onClick (SetState Deleted) ] [], text "Deleted" ]
                ]
            ]
        , if not (List.isEmpty model.errors) then
            ul [] (List.map (\error -> li [] [ text error ]) model.errors)

          else
            text ""
        , button [ onClick Submit, disabled (not (List.isEmpty model.errors) || model.loading) ]
            [ text
                (if model.loading then
                    "Submitting..."

                 else
                    "Submit"
                )
            ]
        , button [ onClick Validate ] [ text "Validate" ]
        ]
