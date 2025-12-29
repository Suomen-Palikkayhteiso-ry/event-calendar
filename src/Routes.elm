module Routes exposing (Route(..), parseUrl, parseCallbackParams)

import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)


type Route
    = Home
    | EventsRoute
    | EventDetail String
    | EditEvent String
    | CreateEvent
    | MapRoute
    | Callback
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map EventsRoute (s "events")
        , Parser.map EventDetail (s "events" </> string)
        , Parser.map EditEvent (s "events" </> string </> s "edit")
        , Parser.map CreateEvent (s "events" </> s "create")
        , Parser.map MapRoute (s "map")
        , Parser.map Callback (s "callback")
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    case url.fragment of
        Just fragment ->
            case Parser.parse routeParser { url | path = fragment, fragment = Nothing } of
                Just route ->
                    route

                Nothing ->
                    NotFound

        Nothing ->
            Home


parseCallbackParams : String -> Maybe ( String, Maybe String )
parseCallbackParams query =
    let
        params =
            String.split "&" query
                |> List.map (String.split "=")
                |> List.filterMap
                    (\parts ->
                        case parts of
                            [ key, value ] ->
                                Just ( key, value )

                            _ ->
                                Nothing
                    )

        findParam name =
            params |> List.filter (\( k, _ ) -> k == name) |> List.head |> Maybe.map Tuple.second

        code =
            findParam "code"

        state =
            findParam "state"
    in
    Maybe.map (\c -> ( c, state )) code