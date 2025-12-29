module CalendarHelpers exposing (addDays, endOfWeek, firstOfMonthPosix, generateDays, generateDaysHelper, lastOfMonthPosix, startOfDayMillis, startOfWeek, weekdayToInt)

import CalendarTypes exposing (CalendarDay)
import DateHelpers
import Time


startOfDayMillis : Time.Posix -> Int
startOfDayMillis posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            Time.toMonth Time.utc posix

        day =
            Time.toDay Time.utc posix
    in
    Time.posixToMillis (DateHelpers.dateToPosix year month day)


firstOfMonthPosix : Int -> Time.Month -> Time.Posix
firstOfMonthPosix year month =
    DateHelpers.dateToPosix year month 1


lastOfMonthPosix : Int -> Time.Month -> Time.Posix
lastOfMonthPosix year month =
    DateHelpers.dateToPosix year month (DateHelpers.daysInMonth year month)


startOfWeek : Time.Posix -> Int -> Time.Posix
startOfWeek posix firstDay =
    let
        weekday =
            Time.toWeekday Time.utc posix

        weekdayInt =
            weekdayToInt weekday

        offset =
            modBy 7 (weekdayInt - firstDay + 7)

        daysBack =
            if offset == 0 then
                0

            else
                offset
    in
    addDays -daysBack posix


endOfWeek : Time.Posix -> Int -> Time.Posix
endOfWeek posix firstDay =
    let
        weekday =
            Time.toWeekday Time.utc posix

        weekdayInt =
            weekdayToInt weekday

        offset =
            modBy 7 (firstDay - weekdayInt + 6)

        daysForward =
            if offset == 6 then
                6

            else
                offset
    in
    addDays daysForward posix


weekdayToInt : Time.Weekday -> Int
weekdayToInt weekday =
    case weekday of
        Time.Mon ->
            1

        Time.Tue ->
            2

        Time.Wed ->
            3

        Time.Thu ->
            4

        Time.Fri ->
            5

        Time.Sat ->
            6

        Time.Sun ->
            0


addDays : Int -> Time.Posix -> Time.Posix
addDays days posix =
    let
        millis =
            Time.posixToMillis posix + round (toFloat days * 24 * 60 * 60 * 1000)
    in
    Time.millisToPosix millis


generateDays : Time.Posix -> Time.Posix -> Time.Month -> List CalendarDay
generateDays start end currentMonth =
    let
        startMillis =
            Time.posixToMillis start

        endMillis =
            Time.posixToMillis end

        days =
            generateDaysHelper startMillis endMillis currentMonth []
    in
    List.reverse days


generateDaysHelper : Int -> Int -> Time.Month -> List CalendarDay -> List CalendarDay
generateDaysHelper currentMillis endMillis currentMonth acc =
    if currentMillis > endMillis then
        acc

    else
        let
            posix =
                Time.millisToPosix currentMillis

            isCurrentMonth =
                Time.toMonth Time.utc posix == currentMonth

            day =
                { date = posix
                , isCurrentMonth = isCurrentMonth
                , events = []
                }

            nextMillis =
                currentMillis + round (24 * 60 * 60 * 1000)
        in
        generateDaysHelper nextMillis endMillis currentMonth (day :: acc)