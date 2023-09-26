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


fadeInDelayLetter : Float
fadeInDelayLetter =
    0.5


fadeInDelayWord : Float
fadeInDelayWord =
    0.7


padding : Int
padding =
    50


verticalSpacing : Int
verticalSpacing =
    5


letterSpacing : Int
letterSpacing =
    15


fontSizeAspectRatio : Float
fontSizeAspectRatio =
    0.67


type alias LetterDetails =
    { letter : String
    , restOfWord : String
    , colour : Color
    , targetLetterFadeInState : AnimationState
    , targetWordFadeInState : AnimationState
    }


tuesday : List LetterDetails
tuesday =
    [ LetterDetails "T" "rain" (rgb255 255 0 0) (FadeInLetter 1) (FadeInWord 1)
    , LetterDetails "U" "ntil" (rgb255 220 100 23) (FadeInLetter 2) (FadeInWord 2)
    , LetterDetails "E" "very" (rgb255 255 255 0) (FadeInLetter 3) (FadeInWord 3)
    , LetterDetails "S" "inue" (rgb255 0 255 0) (FadeInLetter 4) (FadeInWord 4)
    , LetterDetails "D" "evelops" (rgb255 0 0 255) (FadeInLetter 5) (FadeInWord 5)
    , LetterDetails "A" "nd" (rgb255 75 0 130) (FadeInLetter 6) (FadeInWord 6)
    , LetterDetails "Y" "ields" (rgb255 127 0 255) (FadeInLetter 7) (FadeInWord 7)
    ]


type AnimationState
    = NotStarted
    | FadeInLetter Int
    | SlideLeft
    | FadeInWord Int
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
    let
        fadeInQueue =
            Animator.queue
                [ Animator.wait (Animator.seconds 1)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 1)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 2)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 3)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 4)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 5)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 6)
                , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 7)
                , Animator.event (Animator.seconds 1.5) SlideLeft
                , Animator.wait (Animator.seconds 0.2)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 1)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 2)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 3)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 4)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 5)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 6)
                , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord 7)
                ]
    in
    ( { device = device
      , screenSize = screenSize
      , animationState = fadeInQueue <| Animator.init NotStarted
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- (4) - turning out Animator into a subscription
    -- this is where the animator will decide to have a subscription to AnimationFrame or not.
    animator
        |> Animator.toSubscription RuntimeTriggeredAnimationStep model


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
            -- let
            --     _ =
            --         Debug.log "FontSizes: H = " (horizontallyDeterminedFontSize model)
            --     _ =
            --         Debug.log "FontSizes: V = " (verticallyDeterminedFontSize model)
            --     _ =
            --         Debug.log "WWidth: H = " model.screenSize.windowWidth
            --     _ =
            --         Debug.log "WHeight: V = " model.screenSize.windowHeight
            -- in
            ( model |> Animator.update newTime animator
            , Cmd.none
            )


windowWidthWithoutPadding : Model -> Int
windowWidthWithoutPadding model =
    model.screenSize.windowWidth - 2 * padding


horizontallyDeterminedFontSize : Model -> Int
horizontallyDeterminedFontSize model =
    let
        targetWidth =
            toFloat (windowWidthWithoutPadding model - 6 * letterSpacing) / 7
    in
    round <| targetWidth / fontSizeAspectRatio


verticallyDeterminedFontSize : Model -> Int
verticallyDeterminedFontSize model =
    floor <| toFloat (model.screenSize.windowHeight - 2 * padding - 6 * verticalSpacing) / 7


fontSize : Model -> Int
fontSize model =
    min (horizontallyDeterminedFontSize model) (verticallyDeterminedFontSize model)


fontSizeSetByHorizontalConstraint : Model -> Bool
fontSizeSetByHorizontalConstraint model =
    horizontallyDeterminedFontSize model < verticallyDeterminedFontSize model


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.color <| rgb255 0 0 0
        , Font.color <| rgb255 255 255 255
        ]
        (tuesdayLayout model)


tuesdayFont : Attribute Msg
tuesdayFont =
    Font.family
        [ Font.typeface "Virgil"
        , Font.sansSerif
        ]


tuesdayLayout : Model -> Element Msg
tuesdayLayout model =
    column
        [ centerY
        , Element.spacing verticalSpacing
        , Element.padding padding
        ]
        (tuesday
            |> List.indexedMap (letterElement model)
        )


letterElement : Model -> Int -> LetterDetails -> Element Msg
letterElement model letterIndex { letter, restOfWord, colour, targetLetterFadeInState, targetWordFadeInState } =
    el
        [ Font.color colour
        , Font.size <| fontSize model
        , tuesdayFont

        -- , Element.Events.onMouseEnter Hovered
        , moveRight <| rightMoveAmount model letterIndex
        , moveUp <| upMoveAmount model letterIndex
        ]
        (row []
            [ el [ letterFadeInAnimation model targetLetterFadeInState ] <| text letter
            , el [ wordFadeInAnimation model targetWordFadeInState ] <| text restOfWord
            ]
        )


upMoveAmount : Model -> Int -> Float
upMoveAmount model letterIndex =
    let
        windowHeightWithoutPadding =
            toFloat (model.screenSize.windowHeight - 2 * padding)

        verticalOffset =
            if fontSizeSetByHorizontalConstraint model then
                (-3.5 + toFloat letterIndex) * toFloat (fontSize model + verticalSpacing)

            else
                (windowHeightWithoutPadding / 7 * (toFloat letterIndex + 1)) - (windowHeightWithoutPadding / 2)

        preMovement =
            Animator.at <| verticalOffset
    in
    if Animator.current model.animationState == WordsDisplayed then
        0

    else
        Animator.move model.animationState
            (\animationState ->
                case animationState of
                    NotStarted ->
                        preMovement

                    FadeInLetter _ ->
                        preMovement

                    _ ->
                        Animator.at <| 0
            )


rightMoveAmount : Model -> Int -> Float
rightMoveAmount model letterIndex =
    let
        fontWidth =
            round <| fontSizeAspectRatio * toFloat (fontSize model)

        horizontalFontSizePlusSpacing =
            fontWidth + letterSpacing

        leftOffset =
            if fontSizeSetByHorizontalConstraint model then
                0

            else
                toFloat (model.screenSize.windowWidth - 7 * horizontalFontSizePlusSpacing) / 2

        preMovement =
            Animator.at <| toFloat (horizontalFontSizePlusSpacing * letterIndex) + leftOffset
    in
    if Animator.current model.animationState == WordsDisplayed then
        0

    else
        Animator.move model.animationState
            (\animationState ->
                case animationState of
                    NotStarted ->
                        preMovement

                    FadeInLetter _ ->
                        preMovement

                    _ ->
                        Animator.at <| 0
            )


letterFadeInAnimation : Model -> AnimationState -> Attribute Msg
letterFadeInAnimation model targetAnimationState =
    let
        animationComplete =
            case Animator.current model.animationState of
                FadeInLetter currentI ->
                    case targetAnimationState of
                        FadeInLetter targetI ->
                            currentI > targetI

                        _ ->
                            False

                NotStarted ->
                    False

                _ ->
                    True
    in
    htmlAttribute <|
        Animator.Inline.opacity model.animationState
            (\state ->
                if state == targetAnimationState || animationComplete then
                    Animator.at 1

                else
                    Animator.at 0
            )


wordFadeInAnimation : Model -> AnimationState -> Attribute Msg
wordFadeInAnimation model targetAnimationState =
    -- TODO set display: none before we start the animations so they don't cause scrollbars
    let
        animationComplete =
            case Animator.current model.animationState of
                FadeInWord currentI ->
                    case targetAnimationState of
                        FadeInWord targetI ->
                            currentI > targetI

                        _ ->
                            False

                NotStarted ->
                    False

                FadeInLetter _ ->
                    False

                SlideLeft ->
                    False

                WordsDisplayed ->
                    True
    in
    htmlAttribute <|
        Animator.Inline.opacity model.animationState
            (\state ->
                if state == targetAnimationState || animationComplete then
                    Animator.at 1 |> Animator.leaveSmoothly 0.8

                else
                    Animator.at 0
            )
