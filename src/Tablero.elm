module Tablero exposing (init, update, Model, Msg, view)

import Html exposing (Html, li, ul)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs
import Array exposing (toList, Array)
import List
import Debug exposing (toString)
import Dict exposing (Dict)
import Gem exposing (Gem, cssClass)
import Html.Events exposing (onClick)
import Random

import Gem exposing (gemGenerator)


type alias Model = 
    {
        tablero : Matrix Tile
        , level : Int
    }

type Tile = Grass Cover

type Msg = Build Int Int
    | NewGem Tower

type alias Coords =
    { x : Int
    , y : Int
    }

type alias Tower = 
    {
      gem : Gem
    , coords : Coords
    , mvp : Int
    }

type Cover = 
      Stone
    | Gem Tower 
    | Empty


init : Int -> Int -> Model
init rows cols = 
    {
        tablero = Matrix.repeat rows cols <| Grass Empty 
        ,level = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of 
        Build x y ->  (model, newTower model.level x y)

        NewGem tower  -> 
            ( {model | tablero = Matrix.set model.tablero tower.coords.x tower.coords.y (Grass (Gem tower))}, Cmd.none)


newTower : Int -> Int -> Int -> Cmd Msg
newTower level x y  = 
    Random.generate NewGem
        <| Random.map (\gem -> {
                 gem = gem
            , coords = {x = x, y = y}
            , mvp = 0
        })
        <| gemGenerator level


view : Bool -> Model -> Html Msg
view isBuild model = 
    let
        (x, y) = Matrix.size model.tablero
    in
        Html.section 
            [Attrs.class "t-tablero"]
            <| List.concat [
                List.map (\row -> viewRow isBuild (Matrix.getXs model.tablero row) row) (List.range 0 (x - 1))
            ]


viewRow : Bool -> Array Tile -> Int -> Html Msg
viewRow isBuild row rowIndex = 
    ul 
        [ Attrs.class "t-row"]
        (toList (Array.indexedMap (\colIndex tile -> viewTile isBuild tile rowIndex colIndex) row))


viewTile : Bool -> Tile -> Int -> Int -> Html Msg
viewTile isBuild tile rIndex cIndex = 
    let
        isPair = \i -> modBy 2 i == 0 
        
        house = isHouse rIndex cIndex

        isChecked = 
            if isPair rIndex then
                if (not <| isPair cIndex) && not house then
                 [("t-checked", True)] else [] 
            else
                if isPair cIndex && not house then
                 [("t-checked", True)] else []

        coveringElement = case tile of
            Grass cover -> 
                case cover of 
                    Stone -> [("t-stone", True)]
                    Gem tower -> [(cssClass tower.gem, True)]
                    Empty -> []
        
        steps = if (isSteps rIndex cIndex) then [("t-steps", True)] else []

        flag = if (isFlag rIndex cIndex) then [("t-flag", True)] else []

        classes = List.concat [
            [("t-tile", True)]
            , isChecked
            , coveringElement
            , steps
            , flag
            ]
        
        buildable = isBuild 
            && List.isEmpty coveringElement 
            && List.isEmpty flag
            && (not house)
            
    in
        if buildable then
            li [ Attrs.classList classes
            , onClick <| Build rIndex cIndex] [ ]
        else
            li [ Attrs.classList classes] [ ]


isHouse : Int -> Int -> Bool
isHouse x y =
        ( x < 9 && y < 9 )
    || (x > 27 && y > 27)


isSteps : Int -> Int -> Bool
isSteps x y = 
       ((1 < x && x < 20 ) && y == 4)
        || (x == 19 && ( 4 < y && y < 33))
        || ((3 < x && x < 20 ) && y == 32)
        || (x == 4 && ( 18 < y && y < 33))
        || ((3 < x && x < 33 ) && y == 18)
        || (x == 32 && ( 18 < y && y < 33))


isFlag : Int -> Int -> Bool 
isFlag x y = List.any (\ (fx, fy) -> fx == x && fy == y )
    [ (19, 4)
    , (19, 32)
    , (4, 32)
    , (4, 18)
    , (32, 18)]


viewHouse : Html msg
viewHouse = 
    Html.div [] []


viewDoor : Html msg
viewDoor = 
    Html.div [] []