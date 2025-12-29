module Calendar exposing (CalendarDay, Model, Msg, addMonths, dateToPosix, daysInMonth, eventOnDay, init, intToMonth, isLeapYear, monthToInt, update, view)

import CalendarTypes exposing (CalendarDay, Model, Msg(..))
import CalendarView
import DateHelpers
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time
import Types exposing (Event)



-- Re-export types for backward compatibility


type alias CalendarDay =
    CalendarTypes.CalendarDay


type alias Model =
    CalendarTypes.Model


type alias Msg =
    CalendarTypes.Msg



-- Re-export DateHelpers functions for backward compatibility with tests


isLeapYear : Int -> Bool
isLeapYear =
    DateHelpers.isLeapYear


monthToInt : Time.Month -> Int
monthToInt =
    DateHelpers.monthToInt


intToMonth : Int -> Time.Month
intToMonth =
    DateHelpers.intToMonth


daysInMonth : Int -> Time.Month -> Int
daysInMonth =
    DateHelpers.daysInMonth


dateToPosix : Int -> Time.Month -> Int -> Time.Posix
dateToPosix =
    DateHelpers.dateToPosix


eventOnDay : Time.Posix -> Event -> Bool
eventOnDay =
    CalendarView.eventOnDay


init : Model
init =
    { view = "month"
    , events = []
    , currentDate = Time.millisToPosix 1735689600000 -- December 1, 2025
    , locale = "en"
    , firstDay = 1
    }



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetView newView ->
            { model | view = newView }

        SetDate date ->
            { model | currentDate = date }

        SetEvents events ->
            { model | events = events }

        AddEvent event ->
            { model | events = event :: model.events }

        UpdateEvent event ->
            { model
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

        Next ->
            { model | currentDate = addMonths 1 model.currentDate }

        Previous ->
            { model | currentDate = addMonths -1 model.currentDate }



-- Helpers


addMonths : Int -> Time.Posix -> Time.Posix
addMonths months posix =
    DateHelpers.addMonths months posix



-- View


view : Model -> Html Msg
view _ =
    div [ class "calendar" ] [ text "Calendar" ]
