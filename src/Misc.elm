module Misc exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)
import Json.Decode as Decode


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Decode.map tagger keyCode)
