module Api.ChessCom.Player exposing (Player, playerDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)


type alias Player =
    { username : String
    , rating : Int
    , result : String
    , id : String
    }


playerDecoder : Decoder Player
playerDecoder =
    Decode.succeed Player
        |> required "username" string
        |> required "rating" int
        |> required "result" string
        |> required "@id" string
