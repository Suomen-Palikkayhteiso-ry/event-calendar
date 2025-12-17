module Map exposing (..)

import Html exposing (div)
import Html.Attributes exposing (id)
import Ports



-- Model


type alias Model =
    { center : ( Float, Float )
    , zoom : Int
    , marker : Maybe ( Float, Float )
    }


init : Model
init =
    { center = ( 60.1699, 24.9384 ) -- Helsinki
    , zoom = 10
    , marker = Nothing
    }



-- Msg


type Msg
    = InitMap
    | UpdateMap ( Float, Float ) Int (Maybe ( Float, Float ))
    | MarkerMoved ( Float, Float )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitMap ->
            ( model, Ports.initMap { center = model.center, zoom = model.zoom, marker = model.marker } )

        UpdateMap center zoom marker ->
            let
                newModel =
                    { model | center = center, zoom = zoom, marker = marker }
            in
            ( newModel, Ports.updateMap { center = center, zoom = zoom, marker = marker } )

        MarkerMoved pos ->
            ( { model | marker = Just pos }, Cmd.none )



-- View


view : Model -> Html.Html Msg
view model =
    div [ id "map", Html.Attributes.style "height" "400px" ] []
