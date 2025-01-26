module Main exposing (main)

import Animator
import Browser exposing (Document, UrlRequest)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Crossword
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Events
import Element.Font as Font
import Home
import Html exposing (Html)
import Lanky
import Responsive exposing (ScreenSize)
import Route exposing (Route(..))
import Time
import Tuesday
import Url exposing (Url)


type alias Model =
    { page : Page
    , route : Route
    , navKey : Nav.Key
    , device : Device
    , screenSize : ScreenSize
    }


type Msg
    = HomePageMsg Home.Msg
    | TuesdayMsg Tuesday.Msg
    | CrosswordMsg Crossword.Msg
    | LankyMsg Lanky.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | GotNewScreenSize Int Int
    | RuntimeTriggeredAnimationStep Time.Posix


type Subscription
    = HomePageSub (Sub Home.Msg)
    | TuesdaySub (Sub Tuesday.Msg)
    | NoSub


type Page
    = HomePage Home.Model
    | TuesdayPage Tuesday.Model
    | CrosswordPage Crossword.Model
    | LankyPage Lanky.Model
    | NotFoundPage


init : ScreenSize -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { page = NotFoundPage
            , route = Route.parseUrl url
            , navKey = navKey
            , device = classifyDevice { height = flags.windowHeight, width = flags.windowWidth }
            , screenSize = flags
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
                            Home.init model.device model.screenSize
                    in
                    ( HomePage pageModel, Cmd.map HomePageMsg pageCmds )

                Route.Tuesday ->
                    let
                        ( pageModel, pageCmds ) =
                            Tuesday.init model.device model.screenSize
                    in
                    ( TuesdayPage pageModel, Cmd.map TuesdayMsg pageCmds )

                Route.CrosswordToolkit ->
                    let
                        ( pageModel, pageCmds ) =
                            Crossword.init model.device model.screenSize
                    in
                    ( CrosswordPage pageModel, Cmd.map CrosswordMsg pageCmds )

                Route.Lanky ->
                    let
                        ( pageModel, pageCmds ) =
                            Lanky.init ()
                    in
                    ( LankyPage pageModel, Cmd.map LankyMsg pageCmds )
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

                LankyPage _ ->
                    "Lanky's ETA"

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

            CrosswordPage pageModel ->
                Crossword.view pageModel
                    |> Html.map CrosswordMsg

            LankyPage pageModel ->
                Lanky.view pageModel |> Html.map LankyMsg
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSub =
            case model.page of
                CrosswordPage m ->
                    Sub.map CrosswordMsg (Crossword.subscriptions m)

                TuesdayPage m ->
                    Sub.map TuesdayMsg (Tuesday.subscriptions m)

                HomePage m ->
                    Sub.map HomePageMsg (Home.subscriptions m)

                LankyPage m ->
                    Sub.map LankyMsg (Lanky.subscriptions m)

                NotFoundPage ->
                    Sub.none
    in
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotNewScreenSize w h)
        , pageSub
        ]


main : Program ScreenSize Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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

        ( TuesdayMsg subMsg, TuesdayPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Tuesday.update subMsg pageModel
            in
            ( { model | page = TuesdayPage updatedPageModel }
            , Cmd.map TuesdayMsg updatedCmd
            )

        ( LankyMsg subMsg, LankyPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Lanky.update subMsg pageModel
            in
            ( { model | page = LankyPage updatedPageModel }
            , Cmd.map LankyMsg updatedCmd
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

        -- TODO: Deal with page resize
        ( _, _ ) ->
            ( model, Cmd.none )


notFoundView : Html msg
notFoundView =
    Html.h3 [] [ Html.text "Oops! The page you requested was not found!" ]
