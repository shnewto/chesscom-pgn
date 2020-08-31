module Api.ChessCom.Game exposing (..)

import Api.ChessCom.Player exposing (Player, playerDecoder)
import Api.Data as Api
import Http
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required, requiredAt, resolve)


{-| Data Format, each Game:

{
"white": { // details of the white-piece player:
"username": "string", // the username
"rating": 1492, // the player's rating at the start of the game
"result": "string", // see "Game results codes" section
"@id": "string", // URL of this player's profile
},
"black": { // details of the black-piece player:
"username": "string", // the username
"rating": 1942, // the player's rating at the start of the game
"result": "string", // see "Game results codes" section
"@id": "string", // URL of this player's profile
},
"url": "string", // URL of this game
"fen": "string", // final FEN
"pgn": "string", // final PGN
"start\_time": 1254438881, // timestamp of the game start (Daily Chess only)
"end\_time": 1254670734, // timestamp of the game end
"time\_control": "string", // PGN-compliant time control
"rules": "string", // game variant information (e.g., "chess960")
"eco": "string", //URL pointing to ECO opening (if available),
"tournament": "string", //URL pointing to tournament (if available),
"match": "string", //URL pointing to team match (if available)
}

-}
type alias Game =
    { white : Player
    , black : Player
    , url : String
    , fen : String
    , pgn : String
    , startTime : Maybe Int
    , endTime : Maybe Int
    , timeControl : Maybe String
    , rules : String
    , eco : Maybe String
    , tournament : Maybe String
    , match : Maybe String
    }


type alias GameMonth =
    { month : Maybe String
    , year : Maybe String
    , games : List Game
    , visible : Bool
    }


gameURL : String -> String -> String
gameURL username date =
    "https://api.chess.com/pub/player/" ++ username ++ "/games/" ++ date


gamesURL : String -> String
gamesURL username =
    "https://api.chess.com/pub/player/" ++ username ++ "/games/archives"


getGamesURLs : String -> { onResponse : Api.Data (List String) -> msg } -> Cmd msg
getGamesURLs username options =
    Http.get
        { url = gamesURL username
        , expect =
            Api.expectJson options.onResponse gamesURLsDecoder
        }


getGamesInMonth : String -> { onResponse : Api.Data (List Game) -> msg } -> Cmd msg
getGamesInMonth url options =
    Http.get
        { url = url
        , expect = Api.expectJson options.onResponse gamesDecoder
        }


getGameMonths : List String -> { onResponse : Api.Data (List Game) -> msg } -> Cmd msg
getGameMonths urls options =
    let
        allGamesInMonths =
            List.map (\url -> getGamesInMonth url options) urls
    in
    Cmd.batch allGamesInMonths


{-| Data Format:

{
"archives": [
/* array of URLs for monthly archives in ascending chronological order */
]
}

URL pattern: <https://api.chess.com/pub/player/{username}/games/{YYYY}/{MM}>

"YYYY" is the four digit year of the game-end
"MM" is the two-digit month

-}
gamesURLsDecoder : Decode.Decoder (List String)
gamesURLsDecoder =
    let
        toDecoder : List String -> Decode.Decoder (List String)
        toDecoder games =
            Decode.succeed games
    in
    Decode.succeed toDecoder
        |> Pipeline.required "archives" (Decode.list Decode.string)
        |> Pipeline.resolve


{-| URL pattern: <https://api.chess.com/pub/player/{username}/games/{YYYY}/{MM}>
-}
gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> required "white" playerDecoder
        |> required "black" playerDecoder
        |> required "url" string
        |> required "fen" string
        |> required "pgn" string
        |> optional "startTime" (nullable int) Nothing
        |> optional "endTime" (nullable int) Nothing
        |> optional "timeControl" (nullable string) Nothing
        |> required "rules" string
        |> optional "eco" (nullable string) Nothing
        |> optional "tournament" (nullable string) Nothing
        |> optional "match" (nullable string) Nothing


{-| "games": [/* array of game objects */]
-}
gamesDecoder : Decoder (List Game)
gamesDecoder =
    let
        toDecoder : List Game -> Decoder (List Game)
        toDecoder games =
            Decode.succeed games
    in
    Decode.succeed toDecoder
        |> requiredAt [ "games" ] (list gameDecoder)
        |> resolve
