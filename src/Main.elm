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


type Status
    = InitialStatus


type alias Model =
    { status : Status
    }


type Msg
    = NoMsg


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( { status = InitialStatus }, Cmd.none )
        , view = view
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
    column [ alignBottom, paddingXY 500 50, spacing 3 ] [ nameView, subcaptionView ]


view : Model -> Html Msg
view model =
    layout
        [ height fill
        , width fill
        , Background.image "../assets/images/IMG_20161126_162719.jpg"
        , Font.color <| rgb255 255 255 255
        ]
        textView
