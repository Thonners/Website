module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick)


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
    ]


fullIconPath : String -> String
fullIconPath filename =
    String.append iconRoot filename


iconSize : Int
iconSize =
    50


iconRoot : String
iconRoot =
    "../assets/icons/"


type Status
    = InitialStatus


type alias Model =
    { status : Status
    }


type Msg
    = NoMsg


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( { status = InitialStatus }, Cmd.none )
        , view = \model -> { title = "Mathonwy Thomas", body = [ view model ] }
        , update = update
        , subscriptions = \model -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )


scaled : Int -> Float
scaled =
    Element.modular 18 1.25


nameView : Element msg
nameView =
    el [ Font.size (round (scaled 5)), Font.bold ] (text mtName)


subcaptionView : Element msg
subcaptionView =
    el [ Font.size (round (scaled 1)) ] (text mtCaption)


textView : Element msg
textView =
    column
        [ alignBottom
        , paddingXY 500 50
        , spacing 3
        ]
        [ nameView, subcaptionView ]


linksView : Element msg
linksView =
    column
        [ alignRight
        , paddingXY 10 200
        , spacing 10
        , alignTop
        ]
        (List.map
            (\icon ->
                newTabLink []
                    { url = icon.link
                    , label =
                        image
                            [ width <| px iconSize
                            , height <| px iconSize
                            ]
                            { src = fullIconPath icon.filename
                            , description = ""
                            }
                    }
            )
            icons
        )


wrapperView : Element msg
wrapperView =
    row
        [ height fill
        , width fill
        ]
        [ textView
        , linksView
        ]


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.image backgroundImagePath
        , Font.color <| rgb255 255 255 255
        ]
        wrapperView
