module Games.Snake exposing (..)

import Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time
import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode
import Components.Grid as Grid exposing (Board, Row, Position)
import List.Extra as ListE
import List exposing ((::))
import Random


type Direction
    = Left
    | Right
    | Up
    | Down


type Square
    = Empty
    | SnakeBody
    | Candy


type alias Snake =
    List Position


type alias Model =
    { time : Time.Posix
    , snake : Snake
    , candyPosition : Position
    , direction : Direction
    }


type Msg
    = Tick
    | KeyPressed (Maybe Direction)
    | RandomPosition Position


size : (Int, Int)
size =
    ( 10, 10 )


initialState : Model
initialState =
    Model
        (Time.millisToPosix 0)
        [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
        ( 6, 4 )
        Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                newHead =
                    List.head model.snake
                        |> Maybe.withDefault (0,0)
                        |> updatePosition model.direction


                isEatingCandy =
                    newHead == model.candyPosition


                newSnakeTail =
                    if isEatingCandy 
                    then model.snake
                    else model.snake
                        |> ListE.unconsLast
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
                    
                newSnake =
                    newHead :: newSnakeTail

            in
                ( { model | snake = newSnake }
                , if isEatingCandy then generateRandomPositionCmd else Cmd.none
                )


        KeyPressed maybeDirection ->
            ( maybeDirection
                |> Maybe.map (\newDirection -> { model | direction = newDirection })
                |> Maybe.withDefault model
            , Cmd.none
            )

        RandomPosition position ->
            if List.member position model.snake
            then ( model, generateRandomPositionCmd)
            else ( { model | candyPosition = position }, Cmd.none )


generateRandomPositionCmd : Cmd Msg
generateRandomPositionCmd =
    Random.generate RandomPosition randomPositionGenerator


randomPositionGenerator : Random.Generator (Int, Int)
randomPositionGenerator =
    Random.pair
        (Random.int 0 (Tuple.first size - 1))
        (Random.int 0 (Tuple.second size - 1))


updatePosition : Direction -> Position -> Position
updatePosition direction position =
    case direction of
        Up -> Tuple.mapFirst (\p -> p - 1) position
        Down -> Tuple.mapFirst (\p -> p + 1) position
        Left -> Tuple.mapSecond (\p -> p - 1) position
        Right -> Tuple.mapSecond (\p -> p + 1) position


view : Model -> Html Msg
view model =
    div [] 
        [ Grid.initialize size Empty
            |> updateBoardWithSnake model.snake
            |> Grid.updatePosition model.candyPosition Candy
            |> Grid.viewBoard viewSquare
        ]


updateBoardWithSnake : Snake -> Board Square -> Board Square
updateBoardWithSnake snake board =
    case List.head snake of
        Just head ->
            board
                |> Grid.updatePosition head SnakeBody
                |> updateBoardWithSnake (List.tail snake |> Maybe.withDefault [])

        Nothing ->
            board


viewSquare : Square -> Html Msg
viewSquare square =
    let
        squareClass =
            case square of
                Empty -> ""
                SnakeBody -> "snake-body"
                Candy -> "snake-candy"
    in
        div [ class ("square " ++ squareClass) ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 400 (\_ -> Tick)
        , onKeyDown (Decode.map keyPressed (Decode.field "key" Decode.string))
        ]


keyPressed : String -> Msg
keyPressed key =
    if key == "ArrowUp" then KeyPressed (Just Up)
    else if key == "ArrowDown" then KeyPressed (Just Down)
    else if key == "ArrowLeft" then KeyPressed (Just Left)
    else if key == "ArrowRight" then KeyPressed (Just Right)
    else KeyPressed Nothing