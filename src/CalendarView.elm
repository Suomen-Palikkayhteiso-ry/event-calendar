module CalendarView exposing (eventOnDay)

import CalendarHelpers
import DateUtils
import Time
import Types exposing (Event)


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
