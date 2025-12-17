module I18n exposing (..)module I18n exposing (..)


type alias Translations =
    { calendar : String
    , list : String
    , today : String
    , prev : String
    , next_button : String
    -- Add more as needed
    }


fi : Translations
fi =
    { calendar = "kalenteri"
    , list = "lista"
    , today = "tänään"
    , prev = "Edellinen"
    , next_button = "Seuraava"
    }


get : String -> String
get key =
    case key of
        "calendar" -> fi.calendar
        "list" -> fi.list
        "today" -> fi.today
        "prev" -> fi.prev
        "next_button" -> fi.next_button
        _ -> key