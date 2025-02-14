module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | Tuesday
    | CrosswordToolkit
    | Lanky


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Home top
        , map Tuesday (s "tuesday")
        , map CrosswordToolkit (s "crosswordtoolkit")
        , map Lanky (s "lanky")
        ]
