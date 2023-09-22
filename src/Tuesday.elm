module Tuesday exposing (..)

import Animator
import Animator.Inline
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


type AnimationState
    = NotStarted
    | InitialWait
    | SlideLeft
    | WordsDisplayed


type alias Id =
    String


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix


type alias ScreenSize =
    { windowWidth : Int, windowHeight : Int }


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    , animationState : Animator.Timeline AnimationState
    }


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    ( { device = device
      , screenSize = screenSize
      , animationState = Animator.go (Animator.seconds 2) InitialWait <| Animator.init NotStarted
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


updateAnimationState : Model -> AnimationState -> Time.Posix -> Model
updateAnimationState model nextState time =
    { model | animationState = Animator.go (Animator.seconds 5) nextState model.animationState } |> Animator.update time animator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RuntimeTriggeredAnimationStep newTime ->
            if Animator.arrivedAt InitialWait newTime model.animationState then
                let
                    _ =
                        Debug.log "Starting the animation..."
                in
                ( updateAnimationState model SlideLeft newTime, Cmd.none )

            else if Animator.arrivedAt SlideLeft newTime model.animationState && Animator.current model.animationState == SlideLeft then
                let
                    _ =
                        Debug.log "Arrived at SlideLeftComplete: " model.animationState
                in
                ( updateAnimationState model WordsDisplayed newTime, Cmd.none )

            else if Animator.arrivedAt WordsDisplayed newTime model.animationState then
                let
                    _ =
                        Debug.log "Arrived at words displayed: " model.animationState
                in
                ( model |> Animator.update newTime animator, Cmd.none )

            else
                let
                    _ =
                        Debug.log "Tick"
                in
                ( model |> Animator.update newTime animator
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


tuesdayFont : Attribute Msg
tuesdayFont =
    Font.family
        [ Font.typeface "Virgil"
        , Font.sansSerif
        ]


horizontalTuesday : Model -> Element Msg
horizontalTuesday model =
    column
        [ centerY
        , Element.spacing spacing
        , Element.padding padding
        ]
        (tuesday
            |> List.indexedMap (letterElement model)
        )


verticalTuesday : Model -> Element Msg
verticalTuesday model =
    column
        [ centerY
        , Element.spacing spacing
        , Element.padding padding
        ]
        (tuesday
            |> List.indexedMap (letterElement model)
        )


letterElement : Model -> Int -> ( String, String, Color ) -> Element Msg
letterElement model letterIndex ( letter, restOfWord, color ) =
    el
        [ Font.color color
        , Font.size <| verticalFontSize model
        , tuesdayFont

        -- , Element.Events.onMouseEnter Hovered
        , moveRight <| rightMoveAmount model letterIndex
        , moveUp <| upMoveAmount model letterIndex
        ]
        (row []
            [ el [ letterFadeInAnimation model ] <| text letter
            , el [ wordFadeInAnimation model ] <| text restOfWord
            ]
        )


upMoveAmount : Model -> Int -> Float
upMoveAmount model letterIndex =
    let
        windowHeightWithoutPadding =
            toFloat (model.screenSize.windowHeight - 2 * padding)
    in
    if Animator.current model.animationState == WordsDisplayed then
        0

    else
        Animator.move model.animationState
            (\animationState ->
                if animationState == NotStarted || animationState == InitialWait then
                    Animator.at <| (windowHeightWithoutPadding / 7 * (toFloat letterIndex + 1)) - (windowHeightWithoutPadding / 2)

                else
                    Animator.at <| 0
            )


rightMoveAmount : Model -> Int -> Float
rightMoveAmount model letterIndex =
    let
        horizontalFontSize =
            0.66 * toFloat (verticalFontSize model)

        horizontalFontSizePlusPadding =
            horizontalFontSize + toFloat padding

        windowWidthWithoutPadding =
            toFloat (model.screenSize.windowWidth - 2 * padding)

        leftHandOffset =
            windowWidthWithoutPadding / 2 - (3.5 * horizontalFontSizePlusPadding)
    in
    if Animator.current model.animationState == WordsDisplayed then
        0

    else
        Animator.move model.animationState
            (\animationState ->
                if animationState == NotStarted || animationState == InitialWait then
                    Animator.at <| (leftHandOffset + horizontalFontSizePlusPadding * toFloat letterIndex)

                else
                    Animator.at 0
            )


letterFadeInAnimation : Model -> Attribute Msg
letterFadeInAnimation model =
    let
        targetAnimation =
            Animator.Inline.opacity model.animationState
                (\state ->
                    if state == NotStarted then
                        Animator.at 0

                    else
                        Animator.at 1
                )
    in
    htmlAttribute <|
        targetAnimation


wordFadeInAnimation : Model -> Attribute Msg
wordFadeInAnimation model =
    htmlAttribute <|
        Animator.Inline.opacity model.animationState
            (\state ->
                if state == WordsDisplayed then
                    Animator.at 1

                else
                    Animator.at 0
            )
