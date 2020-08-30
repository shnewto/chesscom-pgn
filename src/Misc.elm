module Misc exposing (..)

import Array
import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Http
import Json.Decode as Decode
import List.Extra


monthNumberToMonthName : String -> String
monthNumberToMonthName monthNumber =
    case monthNumber of
        "01" ->
            "January"

        "02" ->
            "February"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "August"

        "09" ->
            "September"

        "10" ->
            "October"

        "11" ->
            "November"

        "12" ->
            "December"

        _ ->
            monthNumber


resultCodeDescription : String -> String
resultCodeDescription code =
    case code of
        "win" ->
            "Win"

        "checkmated" ->
            "Lost by checkmate"

        "agreed" ->
            "Draw agreed"

        "repetition" ->
            "Draw by repetition"

        "timeout" ->
            "Lost by timeout"

        "resigned" ->
            "Lost by resignation"

        "stalemate" ->
            "Draw by stalemate"

        "lose" ->
            "Lost"

        "insufficient" ->
            "Draw by insufficient material"

        "50move" ->
            "Draw by 50-move rule"

        "abandoned" ->
            "Lost when abandoned"

        "kingofthehill" ->
            "Lost when opponent king reached the hill"

        "threecheck" ->
            "Lost when checked for the 3rd time"

        "timevsinsufficient" ->
            "Draw by timeout vs insufficient material"

        "bughousepartnerlose" ->
            "Bughouse partner lost"

        _ ->
            code


gamePathFromUrl : String -> { b | url : String } -> String
gamePathFromUrl username { url } =
    "/"
        ++ username
        ++ "/"
        ++ (String.split "https://www.chess.com/" url
                |> List.Extra.last
                |> Maybe.withDefault ""
           )


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus resp ->
            "Status Code: " ++ String.fromInt resp

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url


{-| URL pattern: <https://api.chess.com/pub/player/{username}/games/{YYYY}/{MM}>
String.split "/" url == [ "https:"
, ""
, "api.chess.com"
,"pub"
,"player"
,"{username}"
,"games"
,"{YYYY}"
,"{MM}"
]
-}
monthFromURL : String -> Maybe String
monthFromURL url =
    String.split "/" url
        |> Array.fromList
        |> Array.get 8


{-| URL pattern: <https://api.chess.com/pub/player/{username}/games/{YYYY}/{MM}>
String.split "/" url == [ "https:"
, ""
, "api.chess.com"
,"pub"
,"player"
,"{username}"
,"games"
,"{YYYY}"
,"{MM}"
]
-}
yearFromURL : String -> Maybe String
yearFromURL url =
    String.split "/" url
        |> Array.fromList
        |> Array.get 7


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Decode.map tagger keyCode)
