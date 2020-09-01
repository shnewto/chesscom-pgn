module Pages.Top exposing (Model, Msg, Params, page)

import Api.ChessCom.Game exposing (Game, GameMonth)
import Api.Data exposing (Data(..))
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra
import Misc exposing (gamePathFromUrl, monthFromURL, monthNumberToMonthName, onKeyUp, resultCodeDescription, yearFromURL)
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
    | GameMonthsLoaded (Data ( String, List Game ))


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


fetchGameMonths : Model -> Cmd Msg
fetchGameMonths model =
    case model.gameUrls of
        Success urls ->
            Api.ChessCom.Game.getGamesByMonthUrls urls { onResponse = GameMonthsLoaded }

        _ ->
            Cmd.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        usernameLoaded keycode =
            let
                updateUrl : String -> Url Params -> Url Params
                updateUrl username url =
                    { url | query = Dict.update "username" (\_ -> Just username) url.query }

                gotUsername : String -> ( Model, Cmd Msg )
                gotUsername username =
                    ( { model | username = Just username, url = updateUrl username model.url }
                    , Cmd.batch
                        [ fetchGameUrls username
                        ]
                    )
            in
            case ( model.username, keycode ) of
                ( Just username, Nothing ) ->
                    gotUsername username

                ( Just username, Just code ) ->
                    if code == 13 then
                        gotUsername username

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

        ToggleSectionVisible key ->
            let
                toggledGameMonths =
                    case model.gameMonths of
                        Success gameMonths ->
                            Success <|
                                Dict.update key
                                    (\gm ->
                                        Maybe.map
                                            (\a ->
                                                { a | visible = not a.visible }
                                            )
                                            gm
                                    )
                                    gameMonths

                        _ ->
                            model.gameMonths
            in
            ( { model | gameMonths = toggledGameMonths }, Cmd.none )

        GameURLsLoaded gameUrls ->
            ( { model | gameUrls = gameUrls, gameMonths = Loading }, fetchGameMonths { model | gameUrls = gameUrls } )

        GameMonthsLoaded gamesInMonth ->
            let
                gameMonth url games =
                    { month = monthFromURL url
                    , year = yearFromURL url
                    , games = games
                    , visible = False
                    , url = url
                    , username = model.username
                    }

                updateMonth : Model -> String -> List Game -> Model
                updateMonth modelToUpdate url games =
                    case modelToUpdate.gameMonths of
                        Success gameMonths ->
                            case Dict.get url gameMonths of
                                Just gameMonthsActual ->
                                    { modelToUpdate
                                        | gameMonths =
                                            Success (Dict.update url (\_ -> Just { gameMonthsActual | games = gameMonthsActual.games ++ games }) gameMonths)
                                    }

                                Nothing ->
                                    { modelToUpdate
                                        | gameMonths =
                                            Success (Dict.insert url (gameMonth url games) gameMonths)
                                    }

                        _ ->
                            modelToUpdate
            in
            case gamesInMonth of
                Success ( url, games ) ->
                    ( updateMonth model url games
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


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
    , body = [ searchView model ]
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
                            if Just game.white.username == m.username then
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
