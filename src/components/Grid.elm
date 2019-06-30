module Components.Grid exposing (..)

import Array exposing (Array)
import Html exposing (Html, div)
import Html.Attributes exposing (class)

type alias Position =
    ( Int, Int )


type alias Board square =
    Array (Row square)


type alias Row square =
    Array square


initialize : (Int, Int) -> square -> Board square
initialize (width, height) initialSquare =
    Array.initialize height (\_ -> initializeRow width initialSquare)


initializeRow : Int -> square -> Row square
initializeRow width initialSquare =
    Array.initialize width (\_ -> initialSquare)


updatePosition : Position -> square -> Board square -> Board square
updatePosition position newSquare board =
    board
        |> Array.indexedMap (updateRow position newSquare)


updateRow : Position -> square -> Int -> Row square -> Row square
updateRow (rowPos, squarePos) newSquare rowIndex row =
    if rowPos == rowIndex 
    then Array.indexedMap (mapUpdateRow squarePos newSquare) row
    else row


mapUpdateRow : Int -> square -> Int -> square -> square
mapUpdateRow squarePos newSquare squareIndex oldSquare =
    if squareIndex == squarePos 
    then newSquare
    else oldSquare


viewBoard : (square -> Html msg) -> Board square -> Html msg
viewBoard viewSquare board =
    board
        |> Array.map (viewRow viewSquare)
        |> Array.toList
        |> div [ class "board" ]


viewRow : (square -> Html msg) -> Row square -> Html msg
viewRow viewSquare row =
    row
        |> Array.map viewSquare
        |> Array.toList
        |> div [ class "board-row" ]