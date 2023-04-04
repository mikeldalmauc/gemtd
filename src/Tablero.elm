module Tablero exposing (init, update, Model, Msg, view, fixTower)

import Html exposing (Html, li, ul, div, text, button, input)
import Matrix exposing (Matrix)
import Html.Attributes as Attrs exposing (class, min, max, value, type_)
import Array exposing (toList, Array)
import List
import Debug exposing (toString)
import Dict exposing (Dict)
import IntDict
import Gem exposing (Gem, cssClass, gemName)
import Html.Events exposing (onClick)
import Random
import Task
import Gem exposing (BasicGem(..), AdvancedGem(..), Gem(..), gemGenerator, gemToString, scaleClass)
import Array exposing (length)
import Gem exposing (getLevel)
import Html exposing (p)


import TableroTypes exposing (..)

import BFS exposing (GraphTablero, initGraphFromTablero)
import Graph exposing (Graph)

import Svg exposing (g)
import BFS exposing (viewGraph)
import Graph exposing (NodeContext, Node)



type alias Model = 
    {
          tablero : Matrix Tile
        , graph : GraphTablero
        , level : Int
        , activeTowers : List Tower
        , buildingTowers : List Tower
        , isBuild : Bool
        , wave : Int
        , selected : Maybe Tower
        , paths: List Path
        , viewPath : Bool
    }


type Msg = 
      Build Int Int
    | NewGem Tower
    | Fix Tower
    | ShowInfo Tower
    | StartWave
    | ViewPath Bool


init : Int -> Int -> Model
init rows cols = 
    let
        tablero =
             Matrix.indexedMap (\x y a -> if isFlag x y then Flag else a)
            <| Matrix.repeat rows cols Empty
        
    in
        {
              tablero = tablero
            , graph = initGraphFromTablero tablero
            , level = 1
            , buildingTowers = []
            , activeTowers = []
            , isBuild = True
            , wave = 0
            , selected = Nothing
            , viewPath = True
            , paths = []
        }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of 
        Build x y -> 
            let
                -- (newModel, validPath, paths) = testPath model_
                newGraph = Graph.remove (BFS.toKey (x, y)) model.graph
                (validPath, paths) = testPath { model | graph = newGraph}
            in
                -- Verify if path is not broken 
                if validPath then
                    case (List.length model.buildingTowers) of 
                        4 -> ({model | paths = paths,  graph=newGraph, isBuild = False}, newTower model.level x y )
                        _ -> ({model | paths = paths, graph=newGraph}, newTower model.level x y  )
                else 
                    (model, Cmd.none)

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
                    , graph = BFS.initGraphFromTablero modelWithFixedTowerAndStones.tablero
                    , isBuild = True -- TO BE REMOVED
                }, Task.perform identity (Task.succeed StartWave))

        StartWave -> 
            ({model | wave = model.wave + 1}, Cmd.none)

        ShowInfo tower -> 
            ( {model | selected = Just tower}, Cmd.none)

        ViewPath toggle -> 
                ({ model | viewPath = toggle}, Cmd.none)

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
            <| List.append
                [ Html.section 
                    [Attrs.class "t-tablero"]
                    <| List.concat [
                        List.map (\row -> viewRow model.isBuild (Matrix.getXs model.tablero row) row model) (List.range 0 (x - 1) )
                    ]
                , Html.section 
                    [Attrs.class "t-info"]
                        [viewTower model]
                , viewPathSearchButton model.viewPath
                ] []
                -- <|  if model.viewPath then
                --         [PathSearch.view model.modelPathSearch |> Html.map PathSearchMsg] 
                --     else []



viewRow : Bool -> Array Tile -> Int -> Model -> Html Msg
viewRow isBuild row rowIndex m = 
    ul 
        [ Attrs.class "t-row"]
        (toList (Array.indexedMap (\colIndex tile -> viewTile isBuild tile rowIndex colIndex m) row))


viewTile : Bool -> Tile -> Int -> Int -> Model -> Html Msg
viewTile isBuild tile rIndex cIndex m = 
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

        (coveringElement, buildable, maybeTower) =
            case tile of
                Stone -> ([("t-stone", True)], True, Nothing) 
                Gem tower -> ([(cssClass tower.gem, True), ("t-gem", True), (scaleClass tower.gem, True)], False, Just tower)
                Empty -> ([], True, Nothing)
                Flag -> ([("t-flag", True)], False, Nothing)
                
        classBuildable = 
            if (isBuild && buildable && (not house)) then
                [("t-buildable", True)]
            else
                []

        classes = List.concat [
            [("t-tile", True)]
            , isChecked
            , steps
            , classBuildable
            ]

        
        otherHtml = [
            --indexes = 
            -- div [Attrs.hidden True] [text <| (toString rIndex) ++" "++ (toString cIndex)] 
            -- , (viewGraphNode tile rIndex cIndex m.graph)
            --  viewPathsNode m rIndex cIndex
            --    if not (List.isEmpty coveringElement) then div[class "t-shadow"][] else div [][]
            ] 
    in
        if (List.length classBuildable > 0) then
            if (List.length coveringElement) > 0 then
                li [ Attrs.classList classes
                , onClick <| Build rIndex cIndex] 
                [ Html.div [Attrs.classList  coveringElement] otherHtml ]
            else
                li [ Attrs.classList classes
                , onClick <| Build rIndex cIndex] otherHtml
        else
            if (List.length coveringElement) > 0 then
                case maybeTower of 
                    Just t -> 
                        li [ Attrs.classList classes , onClick <| ShowInfo t] 
                        [ Html.div [Attrs.classList  coveringElement] otherHtml ]  
                    Nothing -> 
                        li [ Attrs.classList classes ] 
                        [ Html.div [Attrs.classList  coveringElement] otherHtml ]  
                
               
            else
                li [ Attrs.classList classes] otherHtml

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
                        -- , [viewGraph model.graph]
                        -- ,        Dict.values 
                        --         <| Dict.map (\k a -> ul [] 
                        --             <| List.append [text <| toString k] 
                        --             <| List.map (\(wx, wy)-> li [][text <| (toString wx) ++ " " ++ (toString wy)]) a )  
                        --         <| adjacencyList model
                        ]

viewPathSearchButton : Bool -> Html Msg
viewPathSearchButton state = 
    if state then
        button [onClick <| ViewPath (not state), Attrs.class "t-ps-toggle"] [ text "Disable PS view" ]
    else
        button [onClick <| ViewPath (not state),  Attrs.class "t-ps-toggle"] [ text "Enable PS view" ]


viewGraph : GraphTablero -> Html Msg
viewGraph graph = 
    Html.section [class "t-graph"]
    [text <| BFS.viewGraph graph]

viewPathsNode : Model -> Int -> Int -> Html Msg 
viewPathsNode m r c =
    if (List.member (r, c)
        <| List.concatMap Basics.identity m.paths) then
        Html.div [class "t-path"] []
    else 
        Html.div [][]

viewGraphNode : Tile -> Int -> Int -> GraphTablero -> Html Msg
viewGraphNode t rI cI g =
    let
      nodeData = \n -> 
                Html.div [] [ 
                      text <| "id" ++ (toString n.id)
                    , text <| "distance" ++ (toString n.label.vertexDistance)]

      node = \nc -> Html.div [
                             Attrs.class "t-graph-node"
                            , Attrs.style "position" "absolute"
                            , Attrs.style "overflow" "hidden" 
                            , Attrs.style "max-width" "30px"
                            , Attrs.style "max-height" "30px"
                            , Attrs.style "pointer-events" "none"] 
            <|  List.append
                (List.map (\(key, v) -> 
                    Html.div [Attrs.class "t-graph-edge"
                    , Attrs.style "overflow" "hidden"] 
                    [text <| toString key , text <| toString v]
                ) <| IntDict.toList nc.outgoing)
                [nodeData nc.node]
        
      empty =  Html.div [] []
    in
        case (Graph.get (BFS.toKey (rI, cI)) g) of
            Just nc -> node nc
            Nothing -> empty


pathPoint : List (Int, Int)
pathPoint = List.append ((4,4) :: flags) [(32, 32)]


testPath : Model -> (Bool, List Path)
testPath model = 
    let 
        (paths, _) = List.foldl 
            (\p (pathsData,  nextItems)-> 
                case (List.head nextItems) of 
                    Nothing ->  (pathsData,  [])
                    Just head -> 
                        let
                            foundPath = BFS.bfs (BFS.toKey p) (BFS.toKey head) model.graph
                        in 
                            ((foundPath::pathsData), Maybe.withDefault [] <| List.tail nextItems )
                                
            ) 
            ([], Maybe.withDefault [] <| List.tail pathPoint)
            pathPoint
        pathsDataFiltered = List.filter (\l -> not <| List.isEmpty l) paths 
    in
        ((List.length pathPoint) - 1 == (List.length pathsDataFiltered), pathsDataFiltered)



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


flags : List (Int, Int)
flags = [(19, 4), (19, 32), (4, 32), (4, 18), (32, 18)]


flagIndex : (Int, Int) -> Int
flagIndex flag = 
    let
        helper idx lst =
            case lst of
                [] ->
                    0

                x :: xs ->
                    if x == flag then
                        idx
                    else
                        helper (idx + 1) xs
    in
        helper 0 flags

viewHouse : Html msg
viewHouse = 
    Html.div [] []


viewDoor : Html msg
viewDoor = 
    Html.div [] []


-- matrixToList : Matrix a -> List a
-- matrixToList m = 
--     m.
