module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Browser exposing (UrlRequest(..))
import Url exposing (Url)
import Html exposing (Html, nav, div, button, text, h1, ul, li, a, b, br)
import Browser exposing (Document)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class, id)
import Games.Snake as Snake


type alias Model = 
    { nummer : Int
    , url : Url
    , key : Nav.Key
    , snakeModel : Snake.Model
    }


type Msg 
    = UrlChanged Url
    | LinkClicked UrlRequest
    | SnakeMsg Snake.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = initialState
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


initialState : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
initialState _ url key =
    ( Model 0 url key Snake.initialState
    , Cmd.none
    )

-- View

view : Model -> Document Msg
view model =
    Document
        "Homepagina"
        [ navView model
        , Html.map SnakeMsg <| Snake.view model.snakeModel
        ]


navView : Model -> Html Msg
navView model =
    nav []
        [ div [ class "nav-wrapper" ]
            [ ul
                [ id "nav-mobile", class "left hide-on-med-and-down" ]
                [ viewLink "/snake" "Snake"
                ]
            ]
        ]


viewLink : String -> String -> Html Msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        SnakeMsg snakeMsg ->
            Snake.update snakeMsg model.snakeModel
                |> Tuple.mapFirst (\s -> { model | snakeModel = s })
                |> Tuple.mapSecond (Cmd.map SnakeMsg)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Snake.subscriptions model.snakeModel |> Sub.map SnakeMsg
        ]