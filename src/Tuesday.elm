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


tuesday : List ( String, String, Color )
tuesday =
    [ ( "T", "rain", rgb255 255 0 0 )
    , ( "U", "ntil", rgb255 220 100 23 )
    , ( "E", "very", rgb255 255 255 0 )
    , ( "S", "inue", rgb255 0 255 0 )
    , ( "D", "evelops", rgb255 0 0 255 )
    , ( "A", "nd", rgb255 75 0 130 )
    , ( "Y", "ields", rgb255 127 0 255 )
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


type alias ScreenSize =
    { windowWidth : Int, windowHeight : Int }


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    }


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    ( { device = device
      , screenSize = screenSize
      }
    , Cmd.none
    )


padding : Int
padding =
    50


spacing : Int
spacing =
    1


verticalFontSize : Model -> Int
verticalFontSize model =
    round <| toFloat (model.screenSize.windowHeight - 2 * padding - 6 * spacing) / 7


view : Model -> Html Msg
view model =
    let
        tuesdayLayout =
            case model.device.orientation of
                Portrait ->
                    verticalTuesday model

                Landscape ->
                    horizontalTuesday
    in
    layout
        [ height fill
        , width fill
        , Background.color <| rgb255 0 0 0
        , Font.color <| rgb255 255 255 255
        ]
        tuesdayLayout


horizontalTuesday : Element Msg
horizontalTuesday =
    row
        [ centerY, centerX ]
        (tuesday
            |> List.map
                (\( letter, _, color ) ->
                    el
                        [ Font.color color
                        , Font.size 200
                        ]
                        (text letter)
                )
        )


verticalTuesday : Model -> Element Msg
verticalTuesday model =
    column
        [ centerY
        , centerX
        , Element.spacing spacing
        , Element.padding padding
        ]
        (tuesday
            |> List.map
                (\( letter, _, color ) ->
                    el
                        [ Font.color color
                        , Font.size <| verticalFontSize model
                        ]
                        (text letter)
                )
        )
