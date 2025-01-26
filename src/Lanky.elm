module Lanky exposing (..)

import Browser
import Element exposing (Element, centerX, centerY, column, el, layout, text)
import Font exposing (handwritingFont)
import Html exposing (Html, h1)
import Task
import Time exposing (Posix)


updateEvery : Float
updateEvery =
    11


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { remainingTime : Int
    }


type Msg
    = Tick Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model dueDate
    , Task.perform Tick Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every updateEvery Tick


dueDate : Int
dueDate =
    -- Aug 4th in milis since epoch
    1754308800000


remainingTime : Int -> Int
remainingTime currentTime =
    dueDate - currentTime


remainingTimeString : Model -> String
remainingTimeString model =
    let
        totalHours =
            toFloat model.remainingTime / (60 * 60 * 1000)

        hours =
            String.fromInt (floor totalHours)

        totalMinutes =
            (totalHours - toFloat (floor totalHours)) * 60

        minutes =
            String.fromInt (floor totalMinutes)

        totalSeconds =
            (totalMinutes - toFloat (floor totalMinutes)) * 60

        seconds =
            String.fromInt (floor totalSeconds)

        totalMilis =
            remainderBy 1000 model.remainingTime

        milis =
            String.fromInt totalMilis
    in
    hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "." ++ milis


view : Model -> Html Msg
view model =
    layout
        []
        -- [ height fill
        -- , width fill
        -- , Background.color <| rgb255 0 0 0
        -- , Font.color <| rgb255 255 255 255
        -- , handwritingFont
        -- , clipY
        -- ]
        (lankyCountdownLayout model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | remainingTime = remainingTime (Time.posixToMillis newTime) }
            , Cmd.none
            )


flooredDiv : Int -> Float -> Int
flooredDiv numerator denominator =
    floor (toFloat numerator / denominator)


lankyCountdownLayout : Model -> Element Msg
lankyCountdownLayout model =
    column
        [ centerY
        , centerX

        -- , Element.spacing verticalSpacing
        -- , Element.padding padding
        ]
        [ el
            [ handwritingFont
            , centerX
            ]
          <|
            text "Time remaining till Lanky is due:"
        , el
            [ handwritingFont
            , centerX
            ]
          <|
            text (remainingTimeString model)
        ]
