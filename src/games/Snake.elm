module Games.Snake exposing (Model, Msg, initialState, view, update, subscriptions)

import Html exposing (Html, div, text, input, label)
import Html.Attributes exposing (class, value, type_, min, max, step, name, checked)
import Html.Events exposing (onInput)
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


type alias Options =
    { intervalFraction : Float
    , wallsKillSnake : Bool
    }


type alias Model =
    { time : Time.Posix
    , snake : Snake
    , candyPosition : Position
    , direction : Direction
    , nextDirection : Maybe Direction
    , speed : Float
    , options : Options
    }


type Msg
    = Tick
    | KeyPressed (Maybe Direction)
    | RandomPosition Position
    | IntervalFractionChanged String
    | WallsKillSnakeChanged Bool


size : (Int, Int)
size =
    ( 10, 10 )


initialState : Model
initialState =
    { time = (Time.millisToPosix 0)
    , snake = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 3, 0 ) ]
    , candyPosition = ( 6, 4 )
    , direction = Right
    , nextDirection = Nothing
    , speed = 400
    , options = Options 0.95 False
    }


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
                ( { model 
                  | snake = newSnake
                  , speed = if isEatingCandy then model.speed * model.options.intervalFraction else model.speed
                  }
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


        IntervalFractionChanged newFractionString ->
            let
                updateFraction options newFraction =
                    { options | intervalFraction = newFraction }

                newOptions = 
                    String.toFloat newFractionString
                        |> Maybe.map (updateFraction model.options)
                        |> Maybe.withDefault model.options
            in
            
            ( { model | options = newOptions }
            , Cmd.none
            )

        WallsKillSnakeChanged newWallsKillSnake ->
            let
                options =
                    model.options

                newOptions =
                    { options | wallsKillSnake = newWallsKillSnake }
            in
                ( { model | options = newOptions }
                , Cmd.none
                )


generateRandomPositionCmd : Cmd Msg
generateRandomPositionCmd =
    Random.pair (randomBelow (Tuple.first size)) (randomBelow (Tuple.second size))
        |> Random.generate RandomPosition


randomBelow : Int -> Random.Generator Int
randomBelow belowValue =
    Random.int 0 (belowValue - 1)


randomPositionGenerator : Random.Generator (Int, Int)
randomPositionGenerator =
    Random.pair
        (Random.int 0 ((Tuple.first size) - 1))
        (Random.int 0 ((Tuple.second size) - 1))


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
        [ viewSnake model
        , viewOptionsPane model.options
        ]


viewSnake : Model -> Html Msg
viewSnake model =
    Grid.initialize size Empty
        |> updateBoardWithSnake model.snake
        |> Grid.updatePosition model.candyPosition Candy
        |> Grid.viewBoard viewSquare


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


viewOptionsPane : Options -> Html Msg
viewOptionsPane options =
    div []
        [ input 
            [ type_ "number"
            , min "0.01"
            , max "0.99"
            , step "0.01"
            , value (options.intervalFraction |> String.fromFloat)
            , onInput IntervalFractionChanged
            ] 
            []
        , radio model.options.wallsKillSnakee
        ]

radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    label
        [ ]
        [ input [ type_ "radio", name "walls", onInput (\_ -> msg), checked isChecked ] []
        , text value
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every model.speed (\_ -> Tick)
        , onKeyDown (Decode.map keyPressed (Decode.field "key" Decode.string))
        ]


keyPressed : String -> Msg
keyPressed key =
    if key == "ArrowUp" then KeyPressed (Just Up)
    else if key == "ArrowDown" then KeyPressed (Just Down)
    else if key == "ArrowLeft" then KeyPressed (Just Left)
    else if key == "ArrowRight" then KeyPressed (Just Right)
    else KeyPressed Nothing