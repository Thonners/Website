module Tuesday exposing (..)

import Animator
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
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
    | Hovered


type alias ScreenSize =
    { windowWidth : Int, windowHeight : Int }


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    , animationState : Animator.Timeline Bool
    }


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    ( { device = device
      , screenSize = screenSize
      , animationState = Animator.init False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- (4) - turning out Animator into a subscription
    -- this is where the animator will decide to have a subscription to AnimationFrame or not.
    animator
        |> Animator.toSubscription RuntimeTriggeredAnimationStep model


padding : Int
padding =
    50


spacing : Int
spacing =
    1


animator : Animator.Animator Model
animator =
    Animator.animator
        -- Tutorial: https://korban.net/posts/elm/2020-04-07-using-elm-animator-with-elm-ui/
        |> Animator.watching
            -- we tell the animator how
            -- to get the animationState timeline using .animationState
            .animationState
            -- and we tell the animator how
            -- to update that timeline as well
            (\newAnimationState model ->
                { model | animationState = newAnimationState }
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeTriggeredAnimationStep newTime ->
            ( model |> Animator.update newTime animator
            , Cmd.none
            )

        Hovered ->
            let
                _ =
                    Debug.log "Hovered message sent, current status: " model.animationState
            in
            ( { model | animationState = Animator.go Animator.verySlowly True model.animationState }
            , Cmd.none
            )


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
                    horizontalTuesday model
    in
    layout
        [ height fill
        , width fill
        , Background.color <| rgb255 0 0 0
        , Font.color <| rgb255 255 255 255
        ]
        tuesdayLayout


horizontalTuesday : Model -> Element Msg
horizontalTuesday model =
    let
        horizontalFontSize =
            0.66 * toFloat (verticalFontSize model)

        horizontalFontSizePlusPadding =
            horizontalFontSize + toFloat padding

        windowWidthWithoutPadding =
            toFloat (model.screenSize.windowWidth - 2 * padding)

        leftHandOffset =
            windowWidthWithoutPadding / 2 - (3.5 * horizontalFontSizePlusPadding)

        windowHeightWithoutPadding =
            toFloat (model.screenSize.windowHeight - 2 * padding)
    in
    column
        [ centerY
        , Element.spacing spacing
        , Element.padding padding
        ]
        (tuesday
            |> List.indexedMap
                (\i ( letter, _, color ) ->
                    el
                        [ Font.color color
                        , Font.size <| verticalFontSize model
                        , Element.Events.onMouseEnter Hovered
                        , moveRight
                            (Animator.move model.animationState
                                (\animationHasStarted ->
                                    if animationHasStarted then
                                        Animator.at 0

                                    else
                                        Animator.at <| (leftHandOffset + horizontalFontSizePlusPadding * toFloat i)
                                )
                            )
                        , moveUp
                            (Animator.move model.animationState
                                (\animationHasStarted ->
                                    if animationHasStarted then
                                        Animator.at <| 0

                                    else
                                        Animator.at <| (windowHeightWithoutPadding / 7 * (toFloat i + 1)) - (windowHeightWithoutPadding / 2)
                                )
                            )
                        ]
                        (text letter)
                )
        )


verticalTuesday : Model -> Element Msg
verticalTuesday model =
    column
        [ centerY
        , Element.spacing spacing
        , Element.padding padding
        ]
        (tuesday
            |> List.map
                (\( letter, _, color ) ->
                    el
                        [ Font.color color
                        , Font.size <| verticalFontSize model
                        , centerX
                        , Element.Events.onMouseEnter Hovered
                        , moveRight
                            (Animator.move model.animationState
                                (\animationHasStarted ->
                                    if animationHasStarted then
                                        Animator.at 0

                                    else
                                        Animator.at ((toFloat model.screenSize.windowWidth / 2) - 100)
                                )
                            )
                        ]
                        (text letter)
                )
        )
