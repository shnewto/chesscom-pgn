module Pages.Top exposing (Model, Msg, Params, page)

import Api.ChessCom.Game exposing (Game, GameMonth)
import Api.Data exposing (Data(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Misc exposing (gamePathFromUrl, monthNumberToMonthName, onKeyUp, resultCodeDescription)
import Pages.NotFound exposing (Params)
import Shared
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type Msg
    = KeyUp Int
    | UsernameEntered String
    | UsernameLoaded
    | ToggleSectionVisible String
    | GameURLsLoaded (Data (List String))
    | GameMonthsLoaded (Data (List Game))


page : Page Params Model Msg
page =
    Page.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , save = save
        , load = load
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { url : Url Params
    , username : Maybe String
    , gameUrls : Data (List String)
    , errors : List String
    , gameMonths : Data (Dict String GameMonth)
    }


init : Shared.Model -> Url Params -> ( Model, Cmd Msg )
init shared url =
    let
        username =
            Dict.get "username" url.query

        dataState =
            if username == Nothing then
                NotAsked

            else
                Loading

        fetchGamesCmd =
            case username of
                Just name ->
                    fetchGameUrls name

                Nothing ->
                    Cmd.none

        model : Model
        model =
            { url = url
            , username = username
            , gameUrls = dataState
            , gameMonths = dataState
            , errors = []
            }
    in
    ( model
    , Cmd.batch
        [ fetchGamesCmd
        ]
    )


fetchGameUrls : String -> Cmd Msg
fetchGameUrls username =
    Api.ChessCom.Game.getGamesURLs username
        { onResponse = GameURLsLoaded
        }


fetchGameMonths : Model -> List String -> Cmd Msg
fetchGameMonths model =
    case ( model.username, model.gameUrls ) of
        ( Just username, Success urls ) ->
            Api.ChessCom.Game.getGameMonths username urls { onResponse = GameMonthsLoaded }

        _ ->
            Cmd.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        usernameLoaded keycode =
            let
                gotUserName username =
                    ( { model | username = Just username }
                    , Cmd.batch
                        [ fetchGameUrls username
                        ]
                    )
            in
            case ( model.username, keycode ) of
                ( Just username, Nothing ) ->
                    gotUserName username

                ( Just username, Just code ) ->
                    if code == 13 then
                        gotUserName username

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )
    in
    case msg of
        KeyUp keycode ->
            usernameLoaded <| Just keycode

        UsernameEntered username ->
            ( { model
                | username = Just username
              }
            , Cmd.none
            )

        UsernameLoaded ->
            usernameLoaded Nothing

        ToggleSectionVisible s ->
            ( model, Cmd.none )

        GameURLsLoaded gameUrls ->
            ( { model | gameUrls = gameUrls }, fetchGameMonths { model | gameUrls = gameUrls } )

        GameMonthsLoaded gameMonths ->
            { model | gameMonths = gameMonths }


save : Model -> Shared.Model -> Shared.Model
save _ shared =
    shared


load : Shared.Model -> Model -> ( Model, Cmd Msg )
load _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Search"
    , body = [ inputView model.username ]
    }


searchView : Model -> Html Msg
searchView model =
    let
        gamesElement =
            case model.gameMonths of
                Success allGames ->
                    div [] <|
                        List.map
                            (\( _, gameMonth ) -> gameMonthElement gameMonth)
                        <|
                            Dict.toList allGames

                _ ->
                    div [] []
    in
    div [ class "games-body" ]
        [ inputView model.username
        , p [] [ gamesElement ]
        ]


inputView : Maybe String -> Html Msg
inputView placeholderText =
    div [ class "search-view" ]
        [ input
            [ class "username-text-box"
            , type_ "text"
            , placeholder
                (placeholderText
                    |> Maybe.withDefault "Enter a chess.com username..."
                )
            , onKeyUp KeyUp
            , onInput UsernameEntered
            ]
            []
        , input
            [ class "username-submit-button"
            , type_ "submit"
            , onClick <| UsernameLoaded
            ]
            []
        ]


gameMonthElement : GameMonth -> Html Msg
gameMonthElement m =
    let
        monthElement =
            let
                ( icon, active ) =
                    if m.visible then
                        ( String.fromChar (Char.fromCode 0x2182), " active" )

                    else
                        ( String.fromChar (Char.fromCode 0x2180), "" )
            in
            case ( m.month, m.year ) of
                ( Just month, Just year ) ->
                    button
                        [ class <| "collapsible" ++ active
                        , onClick <| ToggleSectionVisible m.url
                        ]
                        [ b [ class "collapsible-icon" ] [ text icon ]
                        , mark [ class "month" ]
                            [ text <|
                                monthNumberToMonthName month
                                    ++ " "
                                    ++ year
                            ]
                        ]

                _ ->
                    div [] []

        gamesElement =
            List.map
                (\game ->
                    let
                        ( userPlayer, opponent ) =
                            if game.white.username == m.username then
                                ( game.white, game.black )

                            else
                                ( game.black, game.white )
                    in
                    div [ class "harpoon" ]
                        [ a [ class "game-url", href <| gamePathFromUrl userPlayer.username game ]
                            [ text <|
                                userPlayer.username
                                    ++ " ("
                                    ++ String.fromInt userPlayer.rating
                                    ++ ")"
                                    ++ " vs. "
                                    ++ opponent.username
                                    ++ " ("
                                    ++ String.fromInt opponent.rating
                                    ++ ") : \""
                                    ++ resultCodeDescription userPlayer.result
                                    ++ "\""
                            ]
                        ]
                )
                m.games

        contentElement =
            let
                active =
                    if m.visible then
                        "-active"

                    else
                        "-hidden"
            in
            div [ class <| "content" ++ active ] [ div [ class "games" ] gamesElement ]
    in
    div [ class "" ] [ monthElement, contentElement ]
