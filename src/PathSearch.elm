module PathSearch exposing (..)

import List
import Dict

type alias Key = (Int, Int)

bfs : Dict Int (Key, List Key) -> Key -> Key -> Key -> List Key -> List Key -> Dict Key Bool ->  Maybe List Key -> Bool
bfs graph src dest actual pred dist visited = 
    let
        vecinos =
            case ( Dict.get actual graph) of 
                Nothing -> []
                Just (_, neighbours) -> neighbours
        
        visited = Dict.insert actual graph

        path = List.append actual
        
    in
        List.filter (\k -> Dict.member k visited) vecinos
        |> List.foldl (\(vx, vy) -> 

            )