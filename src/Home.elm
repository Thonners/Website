module Home exposing (..)

import Animator
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Html exposing (Html)
import Responsive exposing (ScreenSize)
import Time


mtName : String
mtName =
    "MATHONWY THOMAS"


mtCaption : String
mtCaption =
    "<Insert witty caption here>"


backgroundImagePath : String
backgroundImagePath =
    "../assets/images/IMG_20161126_162719.jpg"


type alias Icon =
    { filename : String, link : String }


icons : List Icon
icons =
    [ { filename = "crosswordtoolkit.webp"
      , link = "https://play.google.com/store/apps/details?id=com.thonners.crosswordmaker"
      }
    , { filename = "github.svg"
      , link = "https://github.com/Thonners"
      }
    , { filename = "T.svg"
      , link = "/tuesday"
      }
    ]


fullIconPath : String -> String
fullIconPath filename =
    String.append iconRoot filename


iconSize : Int
iconSize =
    50


iconSizeHovered : Int
iconSizeHovered =
    55


iconRoot : String
iconRoot =
    "../assets/icons/"


type Status
    = InitialStatus


type alias ButtonId =
    String


type alias Model =
    { device : Device
    , screenSize : ScreenSize
    , status : Status
    , linkButtonStates : Animator.Timeline (Dict ButtonId State)
    }


type Msg
    = NoMsg
    | RuntimeTriggeredAnimationStep Time.Posix
    | UserHoveredButton ButtonId
    | UserUnhoveredButton ButtonId


type State
    = Default
    | Hover


animator : Animator.Animator Model
animator =
    Animator.animator
        -- Tutorial: https://korban.net/posts/elm/2020-04-07-using-elm-animator-with-elm-ui/
        |> Animator.watchingWith
            -- we tell the animator how
            -- to get the linkButtonStates timeline using .linkButtonStates
            .linkButtonStates
            -- and we tell the animator how
            -- to update that timeline as well
            (\newButtonStates model ->
                { model | linkButtonStates = newButtonStates }
            )
            -- defines when the animation subscription should run. This is needed, in particular,
            -- for continuous animation of so-called “resting states”, where there is no transition
            -- happening between the underlying state of the model. In my case, I only want the
            -- subscription to run when at least one button is in Hover state, which would allow me
            -- to provide a continuous animation of the button under cursor.
            (\buttonStates -> List.any ((==) Hover) <| Dict.values buttonStates)


init : Device -> ScreenSize -> ( Model, Cmd Msg )
init device screenSize =
    ( { device = device
      , screenSize = screenSize
      , status = InitialStatus
      , linkButtonStates =
            Animator.init <|
                Dict.fromList (List.map (\icon -> ( icon.filename, Default )) icons)
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- (4) - turning out Animator into a subscription
    -- this is where the animator will decide to have a subscription to AnimationFrame or not.
    animator
        |> Animator.toSubscription RuntimeTriggeredAnimationStep model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maybeAlways value =
            Maybe.map (\_ -> value)

        setButtonState id newState =
            Dict.update id (maybeAlways newState) <| Animator.current model.linkButtonStates
    in
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        RuntimeTriggeredAnimationStep newTime ->
            ( model |> Animator.update newTime animator
            , Cmd.none
            )

        UserHoveredButton id ->
            ( { model
                | linkButtonStates =
                    Animator.go Animator.veryQuickly (setButtonState id Hover) model.linkButtonStates
              }
            , Cmd.none
            )

        UserUnhoveredButton id ->
            ( { model
                | linkButtonStates =
                    Animator.go Animator.veryQuickly (setButtonState id Default) model.linkButtonStates
              }
            , Cmd.none
            )


scaled : Int -> Float
scaled =
    Element.modular 18 1.25


headerFontSize : Model -> Int
headerFontSize model =
    let
        divisor =
            if model.device.orientation == Landscape then
                30

            else
                15
    in
    round (toFloat model.screenSize.windowWidth / divisor)


subheaderFontSize : Model -> Int
subheaderFontSize model =
    round (toFloat (headerFontSize model) / 2)


nameView : Model -> Element msg
nameView model =
    el [ Font.size <| headerFontSize model, Font.bold ] (text mtName)


subcaptionView : Model -> Element msg
subcaptionView model =
    el [ Font.size <| subheaderFontSize model ] (text mtCaption)


textView : Model -> Element msg
textView model =
    let
        verticalPadding =
            toFloat model.screenSize.windowHeight / 10 |> round

        horizontalPadding =
            toFloat model.screenSize.windowWidth / 7 |> round
    in
    column
        [ alignBottom
        , paddingXY horizontalPadding verticalPadding
        , spacing 3
        ]
        [ nameView model, subcaptionView model ]


linksView : Model -> Element Msg
linksView model =
    let
        linkButtonSize id =
            round <|
                Animator.linear model.linkButtonStates <|
                    \buttonStates ->
                        Animator.at <|
                            if (Maybe.withDefault Default <| Dict.get id buttonStates) == Hover then
                                iconSizeHovered |> toFloat

                            else
                                iconSize |> toFloat
    in
    column
        [ alignRight
        , paddingXY 15 200
        , spacing 10
        , alignTop
        , width <| px 80
        ]
        (el
            [ centerX
            , Font.center
            , Font.color <| rgb255 30 30 30
            ]
            (text "Projects")
            :: List.map
                (\icon ->
                    newTabLink [ centerX ]
                        { url = icon.link
                        , label =
                            image
                                [ width <| px <| linkButtonSize icon.filename
                                , height <| px <| linkButtonSize icon.filename
                                , centerX
                                , Element.Events.onMouseEnter (UserHoveredButton icon.filename)
                                , Element.Events.onMouseLeave (UserUnhoveredButton icon.filename)
                                ]
                                { src = fullIconPath icon.filename
                                , description = ""
                                }
                        }
                )
                icons
        )


wrapperView : Model -> Element Msg
wrapperView model =
    column
        [ height fill
        , width fill
        ]
        [ linksView model
        , textView model
        ]


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.image backgroundImagePath
        , Font.color <| rgb255 255 255 255
        ]
        (wrapperView model)
