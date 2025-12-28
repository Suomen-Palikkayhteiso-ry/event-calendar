module Calendar exposing (..)

import DateUtils
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Time
import Types exposing (Event)



-- Model


type alias Model =
    { view : String
    , events : List Event
    , currentDate : Time.Posix
    , locale : String
    , firstDay : Int
    }


init : Model
init =
    { view = "month"
    , events = []
    , currentDate = Time.millisToPosix 0 -- TODO: set to current date
    , locale = "en"
    , firstDay = 1
    }



-- Msg


type Msg
    = SetView String
    | SetDate Time.Posix
    | SetEvents (List Event)
    | AddEvent Event
    | UpdateEvent Event
    | Next
    | Previous



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
    let
        year =
            Time.toYear Time.utc posix

        month =
            Time.toMonth Time.utc posix

        day =
            Time.toDay Time.utc posix

        totalMonths =
            monthToInt month + months

        newYear =
            year + (totalMonths // 12)

        newMonthInt =
            modBy 12 totalMonths

        newMonth =
            intToMonth newMonthInt

        daysInNewMonth =
            daysInMonth newYear newMonth

        newDay =
            min day daysInNewMonth
    in
    dateToPosix newYear newMonth newDay


dateToPosix : Int -> Time.Month -> Int -> Time.Posix
dateToPosix year month day =
    -- Approximate calculation, ignoring time zones and leap seconds
    let
        daysSinceEpoch =
            daysSince1970 year month day

        millis =
            daysSinceEpoch * 24 * 60 * 60 * 1000
    in
    Time.millisToPosix millis


daysSince1970 : Int -> Time.Month -> Int -> Int
daysSince1970 year month day =
    let
        years =
            year - 1970

        leapYears =
            (years // 4) - (years // 100) + (years // 400)

        daysInYears =
            years * 365 + leapYears

        daysInMonths =
            daysInMonthsBefore month
                + (if monthToInt month > 1 && isLeapYear year then
                    1

                   else
                    0
                  )
    in
    daysInYears + daysInMonths + day - 1


daysInMonthsBefore : Time.Month -> Int
daysInMonthsBefore month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            31

        Time.Mar ->
            59

        Time.Apr ->
            90

        Time.May ->
            120

        Time.Jun ->
            151

        Time.Jul ->
            181

        Time.Aug ->
            212

        Time.Sep ->
            243

        Time.Oct ->
            273

        Time.Nov ->
            304

        Time.Dec ->
            334


monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Time.Jan ->
            0

        Time.Feb ->
            1

        Time.Mar ->
            2

        Time.Apr ->
            3

        Time.May ->
            4

        Time.Jun ->
            5

        Time.Jul ->
            6

        Time.Aug ->
            7

        Time.Sep ->
            8

        Time.Oct ->
            9

        Time.Nov ->
            10

        Time.Dec ->
            11


intToMonth : Int -> Time.Month
intToMonth int =
    case int of
        0 ->
            Time.Jan

        1 ->
            Time.Feb

        2 ->
            Time.Mar

        3 ->
            Time.Apr

        4 ->
            Time.May

        5 ->
            Time.Jun

        6 ->
            Time.Jul

        7 ->
            Time.Aug

        8 ->
            Time.Sep

        9 ->
            Time.Oct

        10 ->
            Time.Nov

        _ ->
            Time.Dec


daysInMonth : Int -> Time.Month -> Int
daysInMonth year month =
    case month of
        Time.Feb ->
            if isLeapYear year then
                29

            else
                28

        Time.Apr ->
            30

        Time.Jun ->
            30

        Time.Sep ->
            30

        Time.Nov ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 4 year == 0) && (modBy 100 year /= 0 || modBy 400 year == 0)



-- View helpers


type alias CalendarDay =
    { date : Time.Posix
    , isCurrentMonth : Bool
    , events : List Event
    }


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
            startOfWeek firstOfMonth model.firstDay

        endOfWeekDate =
            endOfWeek lastOfMonth model.firstDay

        days =
            generateDays startOfWeekDate endOfWeekDate month
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


firstOfMonthPosix : Int -> Time.Month -> Time.Posix
firstOfMonthPosix year month =
    dateToPosix year month 1


lastOfMonthPosix : Int -> Time.Month -> Time.Posix
lastOfMonthPosix year month =
    dateToPosix year month (daysInMonth year month)


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

            isCurrent =
                Time.toMonth Time.utc posix == currentMonth

            day =
                { date = posix, isCurrentMonth = isCurrent, events = [] }
        in
        generateDaysHelper (currentMillis + 24 * 60 * 60 * 1000) endMillis currentMonth (day :: acc)



-- View


view : Model -> Html.Html Msg
view model =
    let
        days =
            getCalendarDays model

        weeks =
            groupByWeeks days
    in
    div [ class "calendar" ]
        [ div [ class "calendar-header" ]
            [ button [ onClick Previous ] [ text "<" ]
            , h2 [] [ text (monthYearString model.currentDate) ]
            , button [ onClick Next ] [ text ">" ]
            ]
        , div [ class "calendar-grid" ]
            (List.concat
                [ [ div [ class "calendar-week-header" ] (List.map weekdayHeader (List.range 1 7)) ]
                , List.map viewWeek weeks
                ]
            )
        ]


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


weekdayHeader : Int -> Html.Html Msg
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


viewWeek : List CalendarDay -> Html.Html Msg
viewWeek days =
    div [ class "calendar-week" ] (List.map viewDay days)


viewDay : CalendarDay -> Html.Html Msg
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


viewEvent : Event -> Html.Html Msg
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
