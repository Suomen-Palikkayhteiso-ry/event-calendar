module EventList exposing (Model, Msg(..), init, update)

import File exposing (File)
import File.Select as Select
import Task



-- MODEL


type alias Model =
    { currentPage : Int
    , pageSize : Int
    , showImportModal : Bool
    , sortBy : SortBy
    , sortDirection : SortDirection
    , titleFilter : String
    , dateFilter : String
    , statusFilter : String
    }


type SortBy
    = Title
    | Date
    | Status


type SortDirection
    = Asc
    | Desc


init : Model
init =
    { currentPage = 1
    , pageSize = 20
    , showImportModal = False
    , sortBy = Date
    , sortDirection = Desc
    , titleFilter = ""
    , dateFilter = ""
    , statusFilter = ""
    }



-- MSG


type Msg
    = EditEvent String
    | DeleteEvent String
    | SetPage Int
    | ToggleImportModal
    | NoOp
    | SelectKMLFile
    | FileSelected File
    | FileLoaded String
    | SortBy SortBy
    | SetTitleFilter String
    | SetDateFilter String
    | SetStatusFilter String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditEvent _ ->
            ( model, Cmd.none )

        DeleteEvent _ ->
            ( model, Cmd.none )

        SetPage page ->
            ( { model | currentPage = page }, Cmd.none )

        ToggleImportModal ->
            ( { model | showImportModal = not model.showImportModal }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SelectKMLFile ->
            ( model, Select.file [ "application/vnd.google-earth.kml+xml", ".kml" ] FileSelected )

        FileSelected file ->
            ( { model | showImportModal = False }, Task.perform FileLoaded (File.toString file) )

        FileLoaded _ ->
            ( model, Cmd.none )

        SortBy sortBy ->
            let
                newDirection =
                    if model.sortBy == sortBy then
                        if model.sortDirection == Asc then
                            Desc

                        else
                            Asc

                    else
                        Asc
            in
            ( { model | sortBy = sortBy, sortDirection = newDirection, currentPage = 1 }, Cmd.none )

        SetTitleFilter filter ->
            ( { model | titleFilter = filter, currentPage = 1 }, Cmd.none )

        SetDateFilter filter ->
            ( { model | dateFilter = filter, currentPage = 1 }, Cmd.none )

        SetStatusFilter filter ->
            ( { model | statusFilter = filter, currentPage = 1 }, Cmd.none )



-- VIEW
-- HELPERS
