module Tuesday exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick)
import Time


tuesday : List ( String, Color )
tuesday =
    [ ( "T", rgb255 255 0 0 )
    , ( "U", rgb255 220 100 23 )
    , ( "E", rgb255 255 255 0 )
    , ( "S", rgb255 0 255 0 )
    , ( "D", rgb255 0 0 255 )
    , ( "A", rgb255 75 0 130 )
    , ( "Y", rgb255 127 0 255 )
    ]


type State
    = Default
    | Hover


type alias Id =
    String


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix
    | UserHoveredButton Id
    | UserUnhoveredButton Id


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.color <| rgb255 0 0 0
        , Font.color <| rgb255 255 255 255
        ]
        (row
            [ centerY, centerX ]
            (tuesday
                |> List.map
                    (\( letter, color ) ->
                        el
                            [ Font.color color
                            , Font.size 200
                            ]
                            (text letter)
                    )
            )
        )
