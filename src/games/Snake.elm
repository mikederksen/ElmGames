module Games.Snake exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Position =
    ( Int, Int )


type alias Board =
    Array Row


type alias Row =
    Array Square


type Square 
    = Empty
    | SnakeHead


type alias Model =
    { time : Time.Posix
    , board : Array Row
    , headPosition : Position
    , direction : Direction
    }


type Msg
    = Tick Time.Posix
    | KeyPressed (Maybe Direction)


size : (Int, Int)
size =
    ( 10, 10 )


initialState : Model
initialState =
    Model (Time.millisToPosix 0) (initialBoard size) ( 0, 0 ) Right


initialBoard : (Int, Int) -> Board
initialBoard (width, height) =
    Array.initialize height (\_ -> initialRow width)


initialRow : Int -> Row
initialRow width =
    Array.initialize width (\_ -> Empty)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                newHeadPosition =
                    updatePosition model.headPosition model.direction

                newBoard =
                    model.board
                        |> updateBoard model.headPosition Empty
                        |> updateBoard newHeadPosition SnakeHead
            in
                ( { model 
                    | board = newBoard
                    , headPosition = newHeadPosition }
                , Cmd.none
                )

        KeyPressed maybeDirection ->
            ( maybeDirection
                |> Maybe.map (\newDirection -> { model | direction = newDirection })
                |> Maybe.withDefault model
            , Cmd.none
            )


updatePosition : Position -> Direction -> Position
updatePosition position direction =
    case direction of
        Up -> Tuple.mapFirst (\p -> p - 1) position
        Down -> Tuple.mapFirst (\p -> p + 1) position
        Left -> Tuple.mapSecond (\p -> p - 1) position
        Right -> Tuple.mapSecond (\p -> p + 1) position


updateBoard : Position -> Square -> Board -> Board
updateBoard position newSquare board =
    board
        |> Array.indexedMap (updateRow position newSquare)


updateRow : Position -> Square -> Int -> Row -> Row
updateRow (rowPos, squarePos) newSquare rowIndex row =
    if rowPos == rowIndex 
    then (row |> Array.indexedMap (\squareIndex oldSquare -> if squareIndex == squarePos then newSquare else oldSquare))
    else row


view : Model -> Html Msg
view model =
    div [] 
        [ drawBoard model.board
        , case model.direction of
            Up -> text "Up"
            Down -> text "Down"
            Left -> text "Left"
            Right -> text "Right"
        ]


drawBoard : Board -> Html Msg
drawBoard board =
    board
        |> Array.map drawRow
        |> Array.toList
        |> div [ class "snake-board" ]


drawRow : Row -> Html Msg
drawRow row =
    row
        |> Array.map drawSquare
        |> Array.toList
        |> div [ class "snake-row" ]


drawSquare : Square -> Html Msg
drawSquare square =
    let
        squareClass =
            case square of
                Empty -> ""
                SnakeHead -> "snake-head"
    in
        div [ class ("square " ++ squareClass) ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , onKeyDown (Decode.map keyPressed (Decode.field "key" Decode.string))
        ]

keyPressed : String -> Msg
keyPressed key =
    if key == "ArrowUp" then KeyPressed (Just Up)
    else if key == "ArrowDown" then KeyPressed (Just Down)
    else if key == "ArrowLeft" then KeyPressed (Just Left)
    else if key == "ArrowRight" then KeyPressed (Just Right)
    else KeyPressed Nothing