module BFS exposing (..)

import Graph exposing (Graph)
import Matrix exposing (Matrix)
import TableroTypes exposing (..)
import Graph exposing (fromNodesAndEdges)
import Matrix exposing (neighbours)
import Graph.DOT as GraphDot
import Debug exposing (toString)


type alias GraphTablero =  Graph Vertex Int




adjacencyListFromTablero : Matrix Tile -> AdjacencyList
adjacencyListFromTablero tablero = 
    let 
        (sizeX, sizeY) = Matrix.size tablero

        keys = List.concatMap (\xi -> List.map (\yi -> (xi, yi)) <| List.range 0 sizeY) 
            <| List.range 0 sizeX

        (unCovered, covered) = List.foldl (\(xi, yi) (unc, cov)-> 
            if not <| isCovered xi yi tablero then 
                (unc, (xi,yi)::cov) 
            else  
                ( (xi, yi)::unc, cov)
            ) 
            ([], []) 
            keys
                
        vecinos = \mx my -> 
            let 
                xIndexes = if mx == 0 then [0,1] else if mx == sizeX then [0, -1] else [-1, 0, 1]
                yIndexes = if my == 0 then [0,1] else if my == sizeY then [0, -1] else [-1, 0, 1]
            in
                   List.map (\coords -> toKey coords) 
                <| List.filter (\(xi, yi) -> not <| List.member (xi, yi) covered)
                <| List.map (\(xi, yi) -> (xi + mx, yi + my))
                <| List.filter (\(xi, yi) -> not <| xi == yi  || xi == -yi) 
                <| List.concatMap (\xi -> List.map (\yi -> (xi, yi)) yIndexes ) 
                <| xIndexes

    in 
        List.map 
                (\(px, py) -> 
                    ((toKey (px, py)), vecinos px py)
                ) 
            unCovered


initGraphFromTablero : Matrix Tile -> GraphTablero
initGraphFromTablero tablero =
    let
        adjList = adjacencyListFromTablero tablero
    
        graph = Graph.fromNodesAndEdges 
            (List.map (\(id, _) -> {id = id, label=initVertexData}) adjList)
            (List.concatMap (\(fromId, neighbours) -> 
                List.map
                    (\toId -> { from = fromId , to = toId, label = 1})
                    neighbours
                )
                adjList
            ) 
            
    in
        graph

initVertexData : Vertex 
initVertexData =
    { vertexDistance = Nothing
    , vertexPredecessor = Nothing
    }


bfs : Int -> Int -> GraphTablero -> List (Int, Int)
bfs src dest g = 
    let 
        (pathShortest, gUnv) = 
            Graph.guidedBfs
                Graph.alongOutgoingEdges
                (\ctxs val path ->    -- append node labels on discovery
                    let
                        currentNode = List.head ctxs
                    in    
                        if (List.length path > 0 && List.length path < val ) then
                            path
                        else
                            case (List.head ctxs) of
                                Nothing -> path
                                Just nc -> 
                                    if nc.node.id == dest then
                                        ctxs
                                    else 
                                        path
                )
                [src]
                []
                g 
    in
        List.map
        (\nc -> (fromKey nc.node.id) )
        pathShortest


isCovered : Int -> Int -> Matrix Tile -> Bool
isCovered xi yi tablero = 
    case (Matrix.get tablero xi yi) of 
        Nothing -> False
        Just tile -> case tile of 
            Empty -> True
            Stone -> False
            Gem _ -> False
            Flag -> True

toKey : (Int, Int) -> Int
toKey (x, y) = 100 * x + y

fromKey : Int -> (Int, Int) 
fromKey k = (  k //100 , modBy 100 k)


viewGraph : GraphTablero -> String
viewGraph graph = 
    let
        nodeToString = \n -> 
            let
                dist = case n.vertexDistance of
                    Nothing -> "Not measured"
                    Just d -> toString d
            in
                Just dist


        edgeToString = \e -> Nothing

    in
        GraphDot.output nodeToString edgeToString graph

