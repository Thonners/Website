module Main exposing (main)

import Animator
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Home
import Html exposing (Html)
import Route exposing (Route(..))
import Time
import Tuesday
import Url exposing (Url)


type alias Model =
    { page : Page
    , route : Route
    , navKey : Nav.Key
    }


type Msg
    = HomePageMsg Home.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | TuesdayMsg Tuesday.Msg


type Page
    = HomePage Home.Model
    | TuesdayPage Tuesday.Model
    | NotFoundPage


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { page = NotFoundPage
            , route = Route.parseUrl url
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Home ->
                    let
                        ( pageModel, pageCmds ) =
                            Home.init ()
                    in
                    ( HomePage pageModel, Cmd.map HomePageMsg pageCmds )

                Route.Tuesday ->
                    let
                        ( pageModel, pageCmds ) =
                            Tuesday.init ()
                    in
                    ( TuesdayPage pageModel, Cmd.map TuesdayMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    let
        title =
            case model.page of
                TuesdayPage _ ->
                    "TUESDAY"

                _ ->
                    "Mathonwy Thomas"
    in
    { title = title
    , body =
        [ case model.page of
            NotFoundPage ->
                notFoundView

            HomePage pageModel ->
                Home.view pageModel
                    |> Html.map HomePageMsg

            TuesdayPage pageModel ->
                Tuesday.view pageModel
                    |> Html.map TuesdayMsg
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( HomePageMsg subMsg, HomePage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Home.update subMsg pageModel
            in
            ( { model | page = HomePage updatedPageModel }
            , Cmd.map HomePageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )


notFoundView : Html msg
notFoundView =
    Html.h3 [] [ Html.text "Oops! The page you requested was not found!" ]
