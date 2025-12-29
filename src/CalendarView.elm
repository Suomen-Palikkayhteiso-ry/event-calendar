module CalendarView exposing (addDays, assignEventsToDay, endOfWeek, eventOnDay, firstOfMonthPosix, generateDays, generateDaysHelper, getCalendarDays, lastOfMonthPosix, startOfDayMillis, startOfWeek, view, weekdayToInt)

import CalendarHelpers
import CalendarTypes exposing (CalendarDay, Model, Msg(..))
import DateHelpers
import DateUtils
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Icons
import Time
import Types exposing (Event)


view : Model -> Html Msg
view model =
    let
        days =
            getCalendarDays model

        weeks =
            groupByWeeks days
    in
    div [ class "calendar" ]
        [ div [ class "calendar-header" ]
            [ button [ onClick Previous ] [ Icons.chevronLeftIcon ]
            , h2 [] [ text (monthYearString model.currentDate) ]
            , button [ onClick Next ] [ Icons.chevronRightIcon ]
            ]
        , div [ class "calendar-grid" ]
            (List.concat
                [ [ div [ class "calendar-week-header" ] (List.map weekdayHeader (List.range 1 7)) ]
                , List.map viewWeek weeks
                ]
            )
        ]


getCalendarDays : Model -> List CalendarDay
getCalendarDays model =
    let
        year =
            Time.toYear Time.utc model.currentDate

        month =
            Time.toMonth Time.utc model.currentDate

        firstOfMonth =
            firstOfMonthPosix year month

        lastOfMonth =
            lastOfMonthPosix year month

        startOfWeekDate =
            CalendarHelpers.startOfWeek firstOfMonth model.firstDay

        endOfWeekDate =
            CalendarHelpers.endOfWeek lastOfMonth model.firstDay

        days =
            CalendarHelpers.generateDays startOfWeekDate endOfWeekDate month
    in
    List.map (assignEventsToDay model.events) days


assignEventsToDay : List Event -> CalendarDay -> CalendarDay
assignEventsToDay events day =
    let
        dayEvents =
            List.filter (eventOnDay day.date) events
    in
    { day | events = dayEvents }


eventOnDay : Time.Posix -> Event -> Bool
eventOnDay dayPosix event =
    if event.allDay then
        let
            eventStart =
                DateUtils.parseUTCDate event.startDate

            eventEnd =
                event.endDate
                    |> Maybe.map DateUtils.parseUTCDate
                    |> Maybe.withDefault eventStart

            -- Normalize to start of day for comparison
            dayStartMillis =
                startOfDayMillis dayPosix

            eventStartDayMillis =
                startOfDayMillis eventStart

            eventEndDayMillis =
                startOfDayMillis eventEnd
        in
        dayStartMillis >= eventStartDayMillis && dayStartMillis <= eventEndDayMillis

    else
        let
            eventStart =
                DateUtils.parseUTCDate event.startDate

            eventDay =
                Time.toDay Time.utc eventStart

            eventMonth =
                Time.toMonth Time.utc eventStart

            eventYear =
                Time.toYear Time.utc eventStart

            dayDay =
                Time.toDay Time.utc dayPosix

            dayMonth =
                Time.toMonth Time.utc dayPosix

            dayYear =
                Time.toYear Time.utc dayPosix
        in
        eventDay == dayDay && eventMonth == dayMonth && eventYear == dayYear


startOfDayMillis : Time.Posix -> Int
startOfDayMillis =
    CalendarHelpers.startOfDayMillis


firstOfMonthPosix : Int -> Time.Month -> Time.Posix
firstOfMonthPosix =
    CalendarHelpers.firstOfMonthPosix


lastOfMonthPosix : Int -> Time.Month -> Time.Posix
lastOfMonthPosix =
    CalendarHelpers.lastOfMonthPosix


startOfWeek : Time.Posix -> Int -> Time.Posix
startOfWeek =
    CalendarHelpers.startOfWeek


endOfWeek : Time.Posix -> Int -> Time.Posix
endOfWeek =
    CalendarHelpers.endOfWeek


weekdayToInt : Time.Weekday -> Int
weekdayToInt =
    CalendarHelpers.weekdayToInt


addDays : Int -> Time.Posix -> Time.Posix
addDays =
    CalendarHelpers.addDays


generateDays : Time.Posix -> Time.Posix -> Time.Month -> List CalendarDay
generateDays =
    CalendarHelpers.generateDays


generateDaysHelper : Int -> Int -> Time.Month -> List CalendarDay -> List CalendarDay
generateDaysHelper =
    CalendarHelpers.generateDaysHelper


monthYearString : Time.Posix -> String
monthYearString posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            Time.toMonth Time.utc posix
    in
    monthToString month ++ " " ++ String.fromInt year


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Tammikuu"

        Time.Feb ->
            "Helmikuu"

        Time.Mar ->
            "Maaliskuu"

        Time.Apr ->
            "Huhtikuu"

        Time.May ->
            "Toukokuu"

        Time.Jun ->
            "Kesäkuu"

        Time.Jul ->
            "Heinäkuu"

        Time.Aug ->
            "Elokuu"

        Time.Sep ->
            "Syyskuu"

        Time.Oct ->
            "Lokakuu"

        Time.Nov ->
            "Marraskuu"

        Time.Dec ->
            "Joulukuu"


weekdayHeader : Int -> Html Msg
weekdayHeader weekdayInt =
    let
        weekday =
            intToWeekday weekdayInt
    in
    div [ class "calendar-weekday" ] [ text (weekdayToString weekday) ]


intToWeekday : Int -> Time.Weekday
intToWeekday int =
    case int of
        1 ->
            Time.Mon

        2 ->
            Time.Tue

        3 ->
            Time.Wed

        4 ->
            Time.Thu

        5 ->
            Time.Fri

        6 ->
            Time.Sat

        7 ->
            Time.Sun

        _ ->
            Time.Mon


weekdayToString : Time.Weekday -> String
weekdayToString weekday =
    case weekday of
        Time.Mon ->
            "Ma"

        Time.Tue ->
            "Ti"

        Time.Wed ->
            "Ke"

        Time.Thu ->
            "To"

        Time.Fri ->
            "Pe"

        Time.Sat ->
            "La"

        Time.Sun ->
            "Su"


viewWeek : List CalendarDay -> Html Msg
viewWeek days =
    div [ class "calendar-week" ] (List.map viewDay days)


viewDay : CalendarDay -> Html Msg
viewDay day =
    let
        dayClasses =
            if day.isCurrentMonth then
                "calendar-day current-month"

            else
                "calendar-day other-month"
    in
    div [ class dayClasses ]
        [ div [ class "calendar-day-number" ] [ text (String.fromInt (Time.toDay Time.utc day.date)) ]
        , div [ class "calendar-day-events" ] (List.map viewEvent day.events)
        ]


viewEvent : Event -> Html Msg
viewEvent event =
    div [ class "calendar-event" ] [ text event.title ]


groupByWeeks : List CalendarDay -> List (List CalendarDay)
groupByWeeks days =
    case days of
        [] ->
            []

        _ ->
            let
                week =
                    List.take 7 days

                rest =
                    List.drop 7 days
            in
            week :: groupByWeeks rest