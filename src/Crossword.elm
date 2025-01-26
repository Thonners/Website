module Crossword exposing (..)

import Animator
import Animator.Inline
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Font exposing (handwritingFont)
import Html exposing (Html, col)
import Responsive exposing (ScreenSize)
import Time


type Msg
    = RuntimeTriggeredAnimationStep Time.Posix


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    , animationState : Animator.Timeline AnimationState
    }


type AnimationState
    = NotStarted
    | FadeIn
    | FadeInLetter Int
    | SlideLeft
    | FadeInWord Int Int
    | FadeOut Int
    | WordsDisplayed


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    ( { device = device
      , screenSize = screenSize
      , animationState = Animator.init NotStarted
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


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.color <| rgb255 0 0 0
        , Font.color <| rgb255 255 255 255
        , handwritingFont
        , clipY
        ]
        (inputBarLayout model)


inputBarLayout : Model -> Element Msg
inputBarLayout model =
    column
        [ centerY
        , centerX
        , handwritingFont

        -- , Element.spacing verticalSpacing
        -- , Element.padding padding
        ]
        [ el [ handwritingFont ] <| text "Enter search term"
        ]
