module Map exposing (..)

import Html exposing (div)
import Html.Attributes exposing (id)
import Ports
import Types exposing (Event)



-- Model


type alias Model =
    { center : ( Float, Float )
    , zoom : Int
    , events : List Event
    }


init : Model
init =
    { center = ( 60.1699, 24.9384 ) -- Helsinki
    , zoom = 10
    , events = []
    }



-- Msg


type Msg
    = InitMap
    | UpdateMap ( Float, Float ) Int (List Event)
    | SetEvents (List Event)
    | MarkerMoved ( Float, Float )



-- Update


toMapEvent : Event -> { id : String, title : String, point : Maybe { lat : Float, lon : Float } }
toMapEvent event =
    { id = event.id
    , title = event.title
    , point = Maybe.map (\p -> { lat = p.lat, lon = p.lon }) event.point
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitMap ->
            ( model, Ports.initMap { center = model.center, zoom = model.zoom, events = List.map toMapEvent model.events } )

        UpdateMap center zoom events ->
            let
                newModel =
                    { model | center = center, zoom = zoom, events = events }
            in
            ( newModel, Ports.updateMap { center = center, zoom = zoom, events = List.map toMapEvent events } )

        SetEvents events ->
            let
                newModel =
                    { model | events = events }
            in
            ( newModel, Ports.updateMap { center = model.center, zoom = model.zoom, events = List.map toMapEvent events } )

        MarkerMoved pos ->
            ( model, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    div [ id "map", Html.Attributes.style "height" "400px" ] []
