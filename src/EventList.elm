module EventList exposing (Model, Msg(..), init, update, view)

import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Types exposing (Event, EventState(..))



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


view : Model -> List Event -> Html Msg
view model events =
    div [ class "container mx-auto" ]
        [ div [ class "flex justify-between items-center mb-4" ]
            [ h1 [ class "text-2xl font-bold" ] [ text "Events" ]
            , div [ class "flex gap-2" ]
                [ button
                    [ class "bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700"
                    , onClick ToggleImportModal
                    ]
                    [ text "Import KML" ]
                , a
                    [ class "bg-green-600 text-white px-4 py-2 rounded hover:bg-green-700 block"
                    , href "/events/create"
                    ]
                    [ text "Add Event" ]
                ]
            ]
        , if model.showImportModal then
            viewImportModal

          else
            text ""
        , div [ class "mb-4 flex gap-4" ]
            [ input [ type_ "text", placeholder "Filter by title", value model.titleFilter, onInput SetTitleFilter, class "px-3 py-2 border border-gray-300 rounded-md" ] []
            , input [ type_ "text", placeholder "Filter by date", value model.dateFilter, onInput SetDateFilter, class "px-3 py-2 border border-gray-300 rounded-md" ] []
            , select [ onInput SetStatusFilter, class "px-3 py-2 border border-gray-300 rounded-md" ]
                [ option [ value "" ] [ text "All statuses" ]
                , option [ value "draft" ] [ text "Draft" ]
                , option [ value "pending" ] [ text "Pending" ]
                , option [ value "published" ] [ text "Published" ]
                , option [ value "deleted" ] [ text "Deleted" ]
                ]
            ]
        , div [ class "bg-white shadow-md rounded-lg overflow-hidden" ]
            [ table [ class "min-w-full divide-y divide-gray-200" ]
                [ thead [ class "bg-gray-50" ]
                    [ tr []
                        [ th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer", onClick (SortBy Title) ] [ text "Title" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer", onClick (SortBy Date) ] [ text "Date" ]
                        , th [ class "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider cursor-pointer", onClick (SortBy Status) ] [ text "Status" ]
                        , th [ class "px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider" ] [ text "Actions" ]
                        ]
                    ]
                , tbody [ class "bg-white divide-y divide-gray-200" ]
                    (List.map viewEventRow (paginate model.currentPage model.pageSize (filterAndSortEvents model events)))
                ]
            , viewPagination model (List.length (filterAndSortEvents model events))
            ]
        ]


filterAndSortEvents : Model -> List Event -> List Event
filterAndSortEvents model events =
    events
        |> List.filter (matchesFilters model)
        |> sortEvents model.sortBy model.sortDirection


matchesFilters : Model -> Event -> Bool
matchesFilters model event =
    String.contains (String.toLower model.titleFilter) (String.toLower event.title)
        && String.contains (String.toLower model.dateFilter) (String.toLower event.startDate)
        && (model.statusFilter == "" || stateToString event.state == model.statusFilter)


sortEvents : SortBy -> SortDirection -> List Event -> List Event
sortEvents sortBy direction events =
    let
        comparator =
            case sortBy of
                Title ->
                    \a b -> compare (String.toLower a.title) (String.toLower b.title)

                Date ->
                    \a b -> compare a.startDate b.startDate

                Status ->
                    \a b -> compare (stateToString a.state) (stateToString b.state)
    in
    case direction of
        Asc ->
            List.sortWith comparator events

        Desc ->
            List.sortWith (\a b -> comparator b a) events


viewEventRow : Event -> Html Msg
viewEventRow event =
    tr []
        [ td [ class "px-6 py-4 whitespace-nowrap" ]
            [ div [ class "text-sm font-medium text-gray-900" ] [ text event.title ]
            , div [ class "text-sm text-gray-500" ] [ text (Maybe.withDefault "" event.location) ]
            ]
        , td [ class "px-6 py-4 whitespace-nowrap text-sm text-gray-500" ]
            [ text event.startDate ]
        , td [ class "px-6 py-4 whitespace-nowrap" ]
            [ span
                [ class ("px-2 inline-flex text-xs leading-5 font-semibold rounded-full " ++ statusColor event.state) ]
                [ text (stateToString event.state) ]
            ]
        , td [ class "px-6 py-4 whitespace-nowrap text-right text-sm font-medium" ]
            [ a
                [ href ("/events/" ++ event.id ++ "/edit")
                , class "text-indigo-600 hover:text-indigo-900 mr-4"
                ]
                [ text "Edit" ]
            , button
                [ onClick (DeleteEvent event.id)
                , class "text-red-600 hover:text-red-900"
                ]
                [ text "Delete" ]
            ]
        ]


viewPagination : Model -> Int -> Html Msg
viewPagination model totalItems =
    let
        totalPages =
            ceiling (toFloat totalItems / toFloat model.pageSize)
    in
    if totalPages <= 1 then
        text ""

    else
        div [ class "bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6" ]
            [ div [ class "flex-1 flex justify-between sm:hidden" ]
                [ button
                    [ onClick (SetPage (Basics.max 1 (model.currentPage - 1)))
                    , disabled (model.currentPage == 1)
                    , class "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                    ]
                    [ text "Previous" ]
                , button
                    [ onClick (SetPage (Basics.min totalPages (model.currentPage + 1)))
                    , disabled (model.currentPage == totalPages)
                    , class "ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                    ]
                    [ text "Next" ]
                ]
            , div [ class "hidden sm:flex-1 sm:flex sm:items-center sm:justify-between" ]
                [ div []
                    [ p [ class "text-sm text-gray-700" ]
                        [ text "Showing page "
                        , span [ class "font-medium" ] [ text (String.fromInt model.currentPage) ]
                        , text " of "
                        , span [ class "font-medium" ] [ text (String.fromInt totalPages) ]
                        ]
                    ]
                , div []
                    [ nav [ class "relative z-0 inline-flex rounded-md shadow-sm -space-x-px", attribute "aria-label" "Pagination" ]
                        [ button
                            [ onClick (SetPage (Basics.max 1 (model.currentPage - 1)))
                            , disabled (model.currentPage == 1)
                            , class "relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                            ]
                            [ span [ class "sr-only" ] [ text "Previous" ]
                            , text "<"
                            ]
                        , button
                            [ onClick (SetPage (Basics.min totalPages (model.currentPage + 1)))
                            , disabled (model.currentPage == totalPages)
                            , class "relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                            ]
                            [ span [ class "sr-only" ] [ text "Next" ]
                            , text ">"
                            ]
                        ]
                    ]
                ]
            ]


viewImportModal : Html Msg
viewImportModal =
    div [ class "fixed inset-0 z-50 overflow-y-auto", attribute "aria-labelledby" "modal-title", attribute "role" "dialog", attribute "aria-modal" "true" ]
        [ div [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
            [ div [ class "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity", attribute "aria-hidden" "true", onClick ToggleImportModal ] []
            , span [ class "hidden sm:inline-block sm:align-middle sm:h-screen", attribute "aria-hidden" "true" ] [ text "\u{200B}" ]
            , div [ class "inline-block align-bottom bg-white rounded-lg text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full" ]
                [ div [ class "bg-white px-4 pt-5 pb-4 sm:p-6 sm:pb-4" ]
                    [ div [ class "sm:flex sm:items-start" ]
                        [ div [ class "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left w-full" ]
                            [ h3 [ class "text-lg leading-6 font-medium text-gray-900", id "modal-title" ] [ text "Import KML" ]
                            , div [ class "mt-2" ]
                                [ p [ class "text-sm text-gray-500 mb-4" ] [ text "Select a .kml file to import events." ]
                                , div [ class "flex justify-center" ]
                                    [ button
                                        [ class "bg-blue-600 text-white px-4 py-2 rounded hover:bg-blue-700"
                                        , onClick SelectKMLFile
                                        ]
                                        [ text "Select File..." ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "bg-gray-50 px-4 py-3 sm:px-6 sm:flex sm:flex-row-reverse" ]
                    [ button
                        [ type_ "button"
                        , class "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:ml-3 sm:w-auto sm:text-sm"
                        , onClick ToggleImportModal
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            ]
        ]



-- HELPERS


paginate : Int -> Int -> List a -> List a
paginate page size list =
    list
        |> List.drop ((page - 1) * size)
        |> List.take size


statusColor : EventState -> String
statusColor state =
    case state of
        Draft ->
            "bg-gray-100 text-gray-800"

        Pending ->
            "bg-yellow-100 text-yellow-800"

        Published ->
            "bg-green-100 text-green-800"

        Deleted ->
            "bg-red-100 text-red-800"


stateToString : EventState -> String
stateToString state =
    case state of
        Draft ->
            "Draft"

        Pending ->
            "Pending"

        Published ->
            "Published"

        Deleted ->
            "Deleted"
