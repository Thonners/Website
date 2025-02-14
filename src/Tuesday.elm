module Tuesday exposing (..)

import Animator
import Animator.Inline
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Font exposing (handwritingFont)
import Html exposing (Html)
import Responsive exposing (ScreenSize)
import Time


fadeInStagger : Float
fadeInStagger =
    0.1


fadeOutStagger : Float
fadeOutStagger =
    0.1


initialMovementStagger : Float
initialMovementStagger =
    0.1


fadeInDelayLetter : Float
fadeInDelayLetter =
    0.5


fadeInDelayWord : Float
fadeInDelayWord =
    0.7


fadeOutDelayWord : Float
fadeOutDelayWord =
    1.0


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
    , restOfWords : List String
    , colour : Color
    , targetLetterFadeInState : AnimationState
    , targetWordFadeInState : AnimationState
    }


tuesdays : List LetterDetails
tuesdays =
    [ LetterDetails "T" [ "UESDAYS", "rain", "onight", "o", "hrough" ] (rgb255 255 0 0) (FadeInLetter 0) (FadeInWord 0 0)
    , LetterDetails "U" [ "plifts", "ntil", "ltra", "s", "ltimate" ] (rgb255 220 100 23) (FadeInLetter 1) (FadeInWord 0 1)
    , LetterDetails "E" [ "veryone", "very", "lderly", "very", "ffort" ] (rgb255 255 255 0) (FadeInLetter 2) (FadeInWord 0 2)
    , LetterDetails "S" [ "timulating", "inew", "oldiers", "ession", "enescent" ] (rgb255 0 255 0) (FadeInLetter 3) (FadeInWord 0 3)
    , LetterDetails "D" [ "evelopment", "evelops", "ominate", "emands", "udes" ] (rgb255 0 0 255) (FadeInLetter 4) (FadeInWord 0 4)
    , LetterDetails "A" [ "nd", "nd", "thleticism;", "ll", "chieve" ] (rgb255 75 0 130) (FadeInLetter 5) (FadeInWord 0 5)
    , LetterDetails "Y" [ "outhful", "ields", "ouths", "our", "outhful" ] (rgb255 127 0 255) (FadeInLetter 6) (FadeInWord 0 6)
    , LetterDetails "S" [ "pirit", "atisfaction", "truggle", "pirit", "tate" ] (rgb255 127 0 255) (FadeInLetter 7) (FadeInWord 0 7)
    ]


tuesdaysLength : Int
tuesdaysLength =
    List.length tuesdays


type AnimationState
    = NotStarted
    | FadeIn
    | FadeInLetter Int
    | SlideLeft
    | FadeInWord Int Int
    | FadeOut Int
    | WordsDisplayed


type alias Id =
    String


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    , animationState : Animator.Timeline AnimationState
    }


wordRevealAnimationStates : Int -> List (Animator.Step AnimationState)
wordRevealAnimationStates currentIndex =
    [ Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 0)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 1)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 2)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 3)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 4)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 5)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 6)
    , Animator.event (Animator.seconds fadeInDelayWord) (FadeInWord currentIndex 7)
    , Animator.wait (Animator.seconds 0.2)
    , Animator.event (Animator.seconds fadeOutDelayWord) (FadeOut currentIndex)
    , Animator.wait (Animator.seconds 0.2)
    ]


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    let
        fadeInQueue =
            Animator.queue
                ([ Animator.wait (Animator.seconds 1)
                 , Animator.event (Animator.seconds 10) FadeIn

                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 0)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 1)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 2)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 3)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 4)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 5)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 6)
                 --  , Animator.event (Animator.seconds fadeInDelayLetter) (FadeInLetter 7)
                 , Animator.event (Animator.seconds 1.5) SlideLeft
                 , Animator.wait (Animator.seconds 0.2)
                 ]
                    ++ wordRevealAnimationStates 0
                    ++ wordRevealAnimationStates 1
                    ++ wordRevealAnimationStates 2
                    ++ wordRevealAnimationStates 3
                    ++ wordRevealAnimationStates 4
                    ++ wordRevealAnimationStates 5
                )
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
            toFloat (windowWidthWithoutPadding model - (tuesdaysLength - 1) * letterSpacing) / toFloat tuesdaysLength
    in
    round <| targetWidth / fontSizeAspectRatio


verticallyDeterminedFontSize : Model -> Int
verticallyDeterminedFontSize model =
    floor <| toFloat (model.screenSize.windowHeight - 2 * padding - (tuesdaysLength - 1) * verticalSpacing) / toFloat tuesdaysLength


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
        , clipY
        ]
        (tuesdayLayout model)


tuesdayFont : Attribute Msg
tuesdayFont =
    -- Font.family
    --     [ Font.typeface "Virgil"
    --     , Font.sansSerif
    --     ]
    handwritingFont


tuesdayLayout : Model -> Element Msg
tuesdayLayout model =
    column
        [ centerY
        , Element.spacing verticalSpacing
        , Element.padding padding
        ]
        (tuesdays
            |> List.indexedMap (letterElement model)
        )


letterElement : Model -> Int -> LetterDetails -> Element Msg
letterElement model letterIndex { letter, restOfWords, colour, targetLetterFadeInState, targetWordFadeInState } =
    let
        delay =
            toFloat letterIndex * initialMovementStagger
    in
    el
        [ Font.color colour
        , Font.size <| fontSize model
        , tuesdayFont

        -- , Element.Events.onMouseEnter Hovered
        , moveRight <| rightMoveAmount model letterIndex delay
        , moveUp <| upMoveAmount model letterIndex delay
        , letterFadeInAnimation model letterIndex delay
        ]
        (row []
            ((el [] <| text letter)
                :: List.indexedMap
                    (\i restOfWord ->
                        el
                            [ wordFadeInAnimation model targetWordFadeInState i
                            , wordAppearAnimation model targetWordFadeInState i
                            , moveUp <| wordFadeOutAnimation model i
                            ]
                        <|
                            text restOfWord
                    )
                    restOfWords
            )
        )


upMoveAmount : Model -> Int -> Float -> Float
upMoveAmount model letterIndex delay =
    let
        _ =
            Debug.log "Mode up delay = " delay

        _ =
            Debug.log "letter Index = " letterIndex

        windowHeightWithoutPadding =
            toFloat (model.screenSize.windowHeight - 2 * padding)

        verticalOffset =
            if fontSizeSetByHorizontalConstraint model then
                -- 3.5 would be the value to put it in the middle of the screen, but it looks better a bit higher up
                (-1.5 + toFloat letterIndex) * toFloat (fontSize model + verticalSpacing)

            else
                (windowHeightWithoutPadding / toFloat tuesdaysLength * (toFloat letterIndex + 1)) - (windowHeightWithoutPadding / 2)

        preMovement =
            verticalOffset
                |> Animator.at
                |> Animator.leaveLate delay

        --(toFloat letterIndex * initialMovementStagger)
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

                    FadeIn ->
                        preMovement

                    _ ->
                        Animator.at <| 0
            )


rightMoveAmount : Model -> Int -> Float -> Float
rightMoveAmount model letterIndex delay =
    let
        _ =
            Debug.log "Move right delay = " delay

        _ =
            Debug.log "letter Index = " letterIndex

        fontWidth =
            round <| fontSizeAspectRatio * toFloat (fontSize model)

        horizontalFontSizePlusSpacing =
            fontWidth + letterSpacing

        leftOffset =
            if fontSizeSetByHorizontalConstraint model then
                0

            else
                toFloat (model.screenSize.windowWidth - tuesdaysLength * horizontalFontSizePlusSpacing) / 2

        preMovement =
            toFloat (horizontalFontSizePlusSpacing * letterIndex)
                + leftOffset
                |> Animator.at
                |> Animator.leaveLate delay

        --(toFloat letterIndex * initialMovementStagger)
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

                    FadeIn ->
                        preMovement

                    _ ->
                        0
                            |> Animator.at
             -- |> Animator.leaveLate (toFloat letterIndex * initialMovementStagger)
            )


letterFadeInAnimation : Model -> Int -> Float -> Attribute Msg
letterFadeInAnimation model letterIndex delay =
    let
        _ =
            Debug.log "Move right delay = " delay

        _ =
            Debug.log "letter Index = " letterIndex

        fadeIn state =
            case state of
                FadeIn ->
                    -- TODO: Update this properly
                    1
                        |> Animator.at
                        |> Animator.leaveLate delay

                FadeInLetter _ ->
                    Animator.at 0
                        |> Animator.leaveLate delay

                NotStarted ->
                    Animator.at 0
                        |> Animator.leaveLate delay

                SlideLeft ->
                    Animator.at 1
                        |> Animator.leaveLate delay

                FadeOut _ ->
                    Animator.at 1
                        |> Animator.leaveLate delay

                WordsDisplayed ->
                    Animator.at 1
                        |> Animator.leaveLate delay

                FadeInWord _ _ ->
                    Animator.at 1
                        |> Animator.leaveLate delay
    in
    htmlAttribute <|
        Animator.Inline.opacity model.animationState fadeIn



-- htmlAttribute <|
--     Animator.Inline.opacity model.animationState
--         (\state ->
--             if state == targetAnimationState || animationComplete then
--                 Animator.at 1
--             else
--                 Animator.at 0
--         )


wordFadeInAnimation : Model -> AnimationState -> Int -> Attribute Msg
wordFadeInAnimation model targetAnimationState targetWordInListIndex =
    let
        fadeIn state =
            case state of
                FadeIn ->
                    -- TODO: Update this properly
                    Animator.at 0
                        |> Animator.arriveSmoothly 0.5

                FadeInWord wordIndex currentRow ->
                    case targetAnimationState of
                        FadeInWord _ targetRow ->
                            if wordIndex == targetWordInListIndex && currentRow >= targetRow then
                                Animator.at 1
                                    |> Animator.arriveSmoothly 0.5

                            else
                                Animator.at 0
                                    |> Animator.arriveSmoothly 0.5

                        _ ->
                            Animator.at 0
                                |> Animator.arriveSmoothly 0.5

                NotStarted ->
                    Animator.at 0
                        |> Animator.arriveSmoothly 0.5

                FadeInLetter _ ->
                    Animator.at 0
                        |> Animator.arriveSmoothly 0.5

                SlideLeft ->
                    Animator.at 0
                        |> Animator.arriveSmoothly 0.5

                FadeOut _ ->
                    Animator.at 0
                        |> Animator.arriveSmoothly 1
                        |> Animator.arriveEarly 0.05

                WordsDisplayed ->
                    Animator.at 1
    in
    htmlAttribute <|
        Animator.Inline.opacity model.animationState fadeIn


wordFadeOutAnimation : Model -> Int -> Float
wordFadeOutAnimation model wordIndex =
    let
        fadeOut state =
            case state of
                FadeOut indexToFade ->
                    if wordIndex == indexToFade then
                        toFloat -200
                            |> Animator.at
                            |> Animator.leaveSmoothly 1

                    else
                        Animator.at 0

                _ ->
                    Animator.at 0
    in
    Animator.move model.animationState fadeOut


wordAppearAnimation : Model -> AnimationState -> Int -> Attribute Msg
wordAppearAnimation model targetAnimationState targetWordNumber =
    let
        showWord state =
            case state of
                NotStarted ->
                    Animator.at 0

                FadeIn ->
                    -- TODO: Update this properly
                    Animator.at 0

                FadeInLetter _ ->
                    Animator.at 0

                SlideLeft ->
                    Animator.at 0

                FadeInWord wordIndex _ ->
                    case targetAnimationState of
                        FadeInWord _ _ ->
                            if wordIndex == targetWordNumber then
                                Animator.at 1

                            else
                                Animator.at 0

                        _ ->
                            Animator.at 0

                FadeOut currentIndex ->
                    if currentIndex == targetWordNumber then
                        Animator.at 0

                    else
                        Animator.at 0

                WordsDisplayed ->
                    Animator.at 1

        stateValueToString value =
            if value > 0 then
                "flex"

            else
                "none"
    in
    htmlAttribute <|
        Animator.Inline.style model.animationState
            "display"
            stateValueToString
            showWord
