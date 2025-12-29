module Calendar exposing (CalendarDay, Model, Msg, addDays, addMonths, assignEventsToDay, dateToPosix, daysInMonth, endOfWeek, eventOnDay, firstOfMonthPosix, generateDays, generateDaysHelper, getCalendarDays, init, intToMonth, isLeapYear, lastOfMonthPosix, monthToInt, startOfDayMillis, startOfWeek, update, view, weekdayToInt)

import CalendarTypes exposing (CalendarDay, Model, Msg(..))
import CalendarHelpers
import CalendarView
import DateHelpers
import DateUtils
import Html exposing (Html)
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


addDays : Int -> Time.Posix -> Time.Posix
addDays =
    CalendarHelpers.addDays


endOfWeek : Time.Posix -> Time.Posix
endOfWeek posix =
    CalendarHelpers.endOfWeek posix 1


firstOfMonthPosix : Time.Posix -> Time.Posix
firstOfMonthPosix posix =
    let
        year = Time.toYear Time.utc posix
        month = Time.toMonth Time.utc posix
    in
    CalendarHelpers.firstOfMonthPosix year month


lastOfMonthPosix : Time.Posix -> Time.Posix
lastOfMonthPosix posix =
    let
        year = Time.toYear Time.utc posix
        month = Time.toMonth Time.utc posix
    in
    CalendarHelpers.lastOfMonthPosix year month


startOfDayMillis : Time.Posix -> Int
startOfDayMillis =
    CalendarHelpers.startOfDayMillis


startOfWeek : Time.Posix -> Time.Posix
startOfWeek posix =
    CalendarHelpers.startOfWeek posix 1


weekdayToInt : Time.Weekday -> Int
weekdayToInt =
    CalendarHelpers.weekdayToInt


assignEventsToDay : List Event -> CalendarDay -> CalendarDay
assignEventsToDay =
    CalendarView.assignEventsToDay


eventOnDay : Time.Posix -> Event -> Bool
eventOnDay =
    CalendarView.eventOnDay


generateDays : Time.Posix -> Time.Posix -> Time.Month -> List CalendarDay
generateDays =
    CalendarHelpers.generateDays


generateDaysHelper : Int -> Int -> Time.Month -> List CalendarDay -> List CalendarDay
generateDaysHelper =
    CalendarHelpers.generateDaysHelper


getCalendarDays : Model -> List CalendarDay
getCalendarDays =
    CalendarView.getCalendarDays



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
view =
    CalendarView.view
