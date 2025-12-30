module Calendar exposing (CalendarDay, Model, Msg, addMonths, dateToPosix, daysInMonth, eventOnDay, init, intToMonth, isLeapYear, monthToInt, update, view)

import CalendarTypes exposing (CalendarDay, Model, Msg(..))
import CalendarView
import DateHelpers
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
view model =
    div [ class "calendar" ]
        [ calendarHeader model
        , calendarGrid model
        ]


calendarHeader : Model -> Html Msg
calendarHeader model =
    div [ class "calendar-header" ]
        [ button [ class "calendar-header-button", onClick Previous ] [ text "<" ]
        , h2 [] [ text (monthName (Time.toMonth Time.utc model.currentDate) ++ " " ++ String.fromInt (Time.toYear Time.utc model.currentDate)) ]
        , button [ class "calendar-header-button", onClick Next ] [ text ">" ]
        ]


monthName : Time.Month -> String
monthName month =
    case month of
        Time.Jan -> "January"
        Time.Feb -> "February"
        Time.Mar -> "March"
        Time.Apr -> "April"
        Time.May -> "May"
        Time.Jun -> "June"
        Time.Jul -> "July"
        Time.Aug -> "August"
        Time.Sep -> "September"
        Time.Oct -> "October"
        Time.Nov -> "November"
        Time.Dec -> "December"


calendarGrid : Model -> Html Msg
calendarGrid model =
    div [ class "calendar-grid" ]
        (weekdayHeaders :: List.concatMap calendarWeek (groupByWeeks (calendarDays model)))


calendarWeek : List CalendarDay -> List (Html Msg)
calendarWeek week =
    List.map calendarDay week


weekdayHeaders : Html Msg
weekdayHeaders =
    div [ class "calendar-week-header" ]
        [ div [ class "calendar-weekday" ] [ text "Mon" ]
        , div [ class "calendar-weekday" ] [ text "Tue" ]
        , div [ class "calendar-weekday" ] [ text "Wed" ]
        , div [ class "calendar-weekday" ] [ text "Thu" ]
        , div [ class "calendar-weekday" ] [ text "Fri" ]
        , div [ class "calendar-weekday" ] [ text "Sat" ]
        , div [ class "calendar-weekday" ] [ text "Sun" ]
        ]


calendarDay : CalendarDay -> Html Msg
calendarDay day =
    div [ class ("calendar-day" ++ (if day.isCurrentMonth then "" else " other-month")) ]
        [ div [ class "calendar-day-number" ] [ text (String.fromInt (Time.toDay Time.utc day.date)) ]
        , div [ class "calendar-day-events" ] (List.map calendarEvent day.events)
        ]


calendarEvent : Event -> Html Msg
calendarEvent event =
    div [ class "calendar-event" ] [ text event.title ]


groupByWeeks : List a -> List (List a)
groupByWeeks list =
    case list of
        [] -> []
        _ -> List.take 7 list :: groupByWeeks (List.drop 7 list)


calendarDays : Model -> List CalendarDay
calendarDays model =
    let
        year = Time.toYear Time.utc model.currentDate
        month = Time.toMonth Time.utc model.currentDate
        firstDayOfMonth = dateToPosix year month 1
        
        -- Find the first Monday of the calendar grid
        firstDayWeekday = Time.toWeekday Time.utc firstDayOfMonth
        daysToSubtract = case firstDayWeekday of
            Time.Mon -> 0
            Time.Tue -> 1
            Time.Wed -> 2
            Time.Thu -> 3
            Time.Fri -> 4
            Time.Sat -> 5
            Time.Sun -> 6
        
        startDateMillis = Time.posixToMillis firstDayOfMonth - daysToSubtract * 24 * 60 * 60 * 1000
        
        -- Generate 42 days (6 weeks)
        days = List.range 0 41
            |> List.map (\offset -> Time.millisToPosix (startDateMillis + offset * 24 * 60 * 60 * 1000))
    in
    List.map (\date -> 
        { date = date
        , isCurrentMonth = Time.toMonth Time.utc date == month && Time.toYear Time.utc date == year
        , events = List.filter (eventOnDay date) model.events
        }
    ) days
