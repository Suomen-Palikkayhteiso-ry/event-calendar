module CalendarTypes exposing (CalendarDay, Model, Msg(..))

import Time
import Types exposing (Event)


type alias CalendarDay =
    { date : Time.Posix
    , isCurrentMonth : Bool
    , events : List Event
    }


type alias Model =
    { view : String
    , events : List Event
    , currentDate : Time.Posix
    , locale : String
    , firstDay : Int
    }


type Msg
    = SetView String
    | SetDate Time.Posix
    | SetEvents (List Event)
    | AddEvent Event
    | UpdateEvent Event
    | Next
    | Previous
