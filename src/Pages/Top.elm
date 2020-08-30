module Pages.Top exposing (Model, Msg, Params, page)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Misc exposing (onKeyUp)
import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url exposing (Url)


type alias Params =
    ()


type alias Model =
    Url Params


type Msg
    = Never
    | KeyUp Int
    | UsernameEntered String
    | UsernameLoaded


page : Page Params Model Msg
page =
    Page.static
        { view = view
        }



-- VIEW


view : Url Params -> Document Msg
view { params } =
    { title = "Homepage"
    , body = [ inputView Nothing ]
    }


searchView : Maybe String -> Html Msg
searchView username =
    div [ class "archives-body" ]
        [ inputView username
        , p [] [{- archives -}]
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
