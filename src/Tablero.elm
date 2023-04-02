module Tablero exposing (init, update, Model, Msg, view, fixTower)

import Html exposing (Html, li, ul, div, text, button)
import Matrix exposing(Matrix)
import Html.Attributes as Attrs exposing (class)
import Array exposing (toList, Array)
import List
import Debug exposing (toString)
import Dict exposing (Dict)
import Gem exposing (Gem, cssClass, gemName)
import Html.Events exposing (onClick)
import Random
import Task
import Gem exposing (BasicGem(..), AdvancedGem(..), Gem(..), gemGenerator, gemToString)
import Array exposing (length)
import Gem exposing (getLevel)



type alias Coords =
    { x : Int
    , y : Int
    }

type alias Tower = 
    {
      gem : Gem
    , coords : Coords
    , mvp : Int
    , auras : List String
    , range : Int
    , dmg : Int
    , speed : Int
    }

type Tile = 
      Stone
    | Gem Tower 
    | Empty

type alias Model = 
    {
          tablero : Matrix Tile
        , level : Int
        , activeTowers : List Tower
        , buildingTowers : List Tower
        , isBuild : Bool
        , wave : Int
        , selected : Maybe Tower
    }


type Msg = 
      Build Int Int
    | NewGem Tower
    | Fix Tower
    | ShowInfo Tower
    | StartWave


init : Int -> Int -> Model
init rows cols = 
    {
        tablero = Matrix.repeat rows cols Empty 
        , level = 1
        , buildingTowers = []
        , activeTowers = []
        , isBuild = True
        , wave = 0
        , selected = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of 
        Build x y -> case (List.length model.buildingTowers) of 
            4 -> ({model | isBuild = False} , newTower model.level x y)
            _ -> (model, newTower model.level x y)

        NewGem tower  -> 
            ( { model | tablero = Matrix.set model.tablero tower.coords.x tower.coords.y (Gem tower)
                , buildingTowers = tower :: model.buildingTowers} , Cmd.none)

        Fix tower -> 
            let
                modelWithFixedTowerAndStones = fixTower tower model  
            in
                ( {modelWithFixedTowerAndStones | 
                    activeTowers = tower :: model.activeTowers
                    , buildingTowers = []
                    , isBuild = True -- TO BE REMOVED
                }, Task.perform identity (Task.succeed StartWave))

        StartWave -> 
            ({model | wave = model.wave + 1}, Cmd.none)

        ShowInfo tower -> 
            ( {model | selected = Just tower}, Cmd.none)


newTower : Int -> Int -> Int -> Cmd Msg
newTower level x y  = 
    Random.generate NewGem
        <| Random.map (\gem -> {
                 gem = gem
            , coords = {x = x, y = y}
            , mvp = 0
            , auras = []
            , range = 200
            , dmg = 100
            , speed = 1
            }
        )
        <| gemGenerator level


fixTower : Tower -> Model -> Model
fixTower tower model = 
    List.foldl 
        (\t nextModel -> 
            if tower.coords.x == t.coords.x && tower.coords.y == t.coords.y then
                {nextModel | tablero = Matrix.set nextModel.tablero t.coords.x t.coords.y (Gem tower)}
            else
                {nextModel | tablero = Matrix.set nextModel.tablero t.coords.x t.coords.y Stone}
             ) 
        model 
        model.buildingTowers


view : Model -> Html Msg
view model = 
    let
        (x, y) = Matrix.size model.tablero
    in
        Html.section 
            [Attrs.class "t-frame"]
        [ Html.section 
            [Attrs.class "t-tablero"]
            <| List.concat [
                List.map (\row -> viewRow model.isBuild (Matrix.getXs model.tablero row) row) (List.range 0 (x - 1))
            ]
        , Html.section 
            [Attrs.class "t-info"]
                [viewTower model]
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
        
        steps = if (isSteps rIndex cIndex) then [("t-steps", True)] else []

        flag = if (isFlag rIndex cIndex) then [("t-flag", True)] else []
            
        (coveringElement, notGem, maybeTower) =
            case tile of
                Stone -> ([("t-stone", True)], True, Nothing) 
                Gem tower -> ([(cssClass tower.gem, True), ("t-gem", True)], False, Just tower)
                Empty -> ([], True, Nothing)

        buildable = 
            if (isBuild && notGem && (List.isEmpty flag) && (not house)) then
                [("t-buildable", True)]
            else
                []

        classes = List.concat [
            [("t-tile", True)]
            , isChecked
            , steps
            , flag
            , buildable
            ]

        indexes = div [Attrs.hidden True] [text <| (toString rIndex) ++" "++ (toString cIndex)]
    in
        if (List.length buildable > 0) then
            if (List.length coveringElement) > 0 then
                li [ Attrs.classList classes
                , onClick <| Build rIndex cIndex] 
                [ Html.div [Attrs.classList  coveringElement] [indexes] ]
            else
                li [ Attrs.classList classes
                , onClick <| Build rIndex cIndex] [ indexes]
        else
            if (List.length coveringElement) > 0 then
                case maybeTower of 
                    Just t -> 
                        li [ Attrs.classList classes , onClick <| ShowInfo t] 
                        [ Html.div [Attrs.classList  coveringElement] [indexes] ]  
                    Nothing -> 
                        li [ Attrs.classList classes ] 
                        [ Html.div [Attrs.classList  coveringElement] [indexes] ]  
                
               
            else
                li [ Attrs.classList classes] [ indexes]

-- viewTowerList : List Tower -> Html Msg
-- viewTowerList towers =
--     ul [ class "tower-list" ]
--         <| List.map viewTower towers


viewTower : Model -> Html Msg
viewTower  model =
    case model.selected of 
        Nothing -> Html.div [] []
        Just tower ->
            let
                selectButton = 
                    if (List.any (\t -> t.coords.x == tower.coords.x && t.coords.y == tower.coords.y) model.buildingTowers) then
                        [button [Attrs.disabled <| not ( List.length model.buildingTowers == 5)
                            , onClick (Fix tower)
                            ][text "Select"]]
                    else
                        []
            in
                div [class "selected-info"]
                    <| List.concat 
                        [[Html.h2 [] [text "Info"] ]
                        ,   [li []
                            [ div []
                                [ text <| "Gem: " ++  (gemToString  tower.gem) ]
                            , div []
                                [ text <| "Coords: (" ++ toString tower.coords.x ++ "," ++ toString tower.coords.y ++ ")" ]
                            , div []
                                [ text <| "MVP: " ++ toString tower.mvp ]
                            , div []
                                [ text <| "Auras: " ++ String.join ", " tower.auras ]
                            , div []
                                [ text <| "Range: " ++ toString tower.range ]
                            , div []
                                [ text <| "Damage: " ++ toString tower.dmg ]
                            , div []
                                [ text <| "Speed: " ++ toString tower.speed ]
                            ]]
                        , selectButton
                        ]

{-


-}
calcularCamino : Model -> Maybe List Coords
calcularCamino model = 
    Matrix.model.tablero



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
isFlag x y = List.any (\ (fx, fy) -> fx == x && fy == y ) flags

flags : List (x, y)
flags = [ (19, 4) , (19, 32) , (4, 32) , (4, 18) , (32, 18)]

viewHouse : Html msg
viewHouse = 
    Html.div [] []


viewDoor : Html msg
viewDoor = 
    Html.div [] []