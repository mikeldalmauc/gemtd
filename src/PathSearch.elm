module PathSearch exposing (..)

import Platform 
import List
import Html.Attributes as Attrs
import Html exposing (Html, div, li, text, h2, ul)
import Dict exposing (Dict)
import Debug exposing (toString)
import Set exposing (Set)
import Process
import Task

type alias Key = (Int, Int)


type alias Graph = Dict Key (List Key) 

type alias Model = List PathSearch 

type alias PathSearch = {
      distances : Dict Key (Int, Maybe Key)
    , visited : Set Key
    , unvisited : Set Key
    , graph : Dict Key (List Key)
    , path : Maybe (List Key)
    }

type Msg = 
      Generate


init : Graph -> PathSearch
init graph = 
    {
      distances = Dict.map (\_ _-> (largest, Nothing) ) graph
    , visited = Set.empty
    , unvisited = Set.fromList <| Dict.keys graph
    , graph = graph
    , path = Nothing
    }


update :Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        Generate -> 
            (model, Cmd.none)


largest : Int
largest = 2^31 - 1


bfs : PathSearch -> Key -> Key -> (Maybe Key, Int) -> PathSearch
bfs model src dest (prev,  prevDistance) = 
    let
        newModel = {model | 
              visited = Set.insert src model.visited
            , unvisited = Set.remove src model.unvisited
            , distances = Dict.insert src (prevDistance + 1, prev) model.distances
            }
    in
        case model.path of 
            Just somePath -> 
                if (List.length somePath) <=  prevDistance then
                    newModel
                else
                    searchStep newModel src dest (Nothing, prevDistance + 1)
            Nothing -> searchStep newModel src dest (Nothing, prevDistance + 1)


searchStep :  PathSearch -> Key -> Key -> (Maybe Key, Int) -> PathSearch
searchStep model src dest (prev,  prevDistance) = 
    if src == dest then
        let
            path = getPath dest [] model.distances
        in    
            case model.path of 
                Nothing -> {model | path = Just path}
                Just somePath -> 
                    if (List.length somePath) > (List.length path) then
                        {model | path = Just path}
                    else
                        {model | path = Just somePath}
    else
        let
            vecinos =
                case ( Dict.get src model.graph) of 
                    Nothing -> []
                    Just neighbours -> neighbours
            unvisited = List.filter (\k -> Set.member k model.unvisited) vecinos
        in
            List.foldl (\(xv, yv) prevModel -> 
                        bfs prevModel (xv, yv) dest (Just (xv, yv), prevDistance + 1)
                ) model unvisited


getPath : Key -> List Key -> Dict Key (Int, Maybe Key) -> List Key
getPath act path dict =
    case (Dict.get act dict) of
        Just (_, (Just next)) -> getPath next (act :: path) dict
        Just (_, Nothing) -> act::path
        Nothing -> path


delay : Float -> msg -> Cmd msg
delay time msg =    
    -- create a task that sleeps for `time`
    Process.sleep time
        |> -- once the sleep is over, ignore its output (using `always`)
           -- and then we create a new task that simply returns a success, and the msg
           Task.andThen (always <| Task.succeed msg)
        |> -- finally, we ask Elm to perform the Task, which
           -- takes the result of the above task and
           -- returns it to our update function
           Task.perform identity


-- bfsView : Model -> Html msg
-- bfsView model = 



-- viewPath : Path 
-- viewPath 

view : Model -> Html Msg
view model = 
    Html.div [Attrs.class "t-pathSearch"] 
        <| List.map viewPathSearch model

viewPathSearch : PathSearch -> Html Msg
viewPathSearch model =
    let
        distanceList =
            Dict.toList model.distances

        visitedList =
            Set.toList model.visited
    in
    div []
        [ h2 [] [ text "Distances" ]
        , ul [] <| List.map formatDistance distanceList
        , h2 [] [ text "Visited" ]
        , ul [] <| List.map formatKey visitedList
        , h2 [] [ text "Path"]
        , ul [] <| [formatPaths model.path]
        ]

formatDistance : ( Key, ( Int, Maybe Key ) ) -> Html Msg
formatDistance ( key, ( distance, maybePrev ) ) =
    li []
        [ text <| toString key ++ ": " ++ toString distance
        , case maybePrev of
            Just prev ->
                text <| " (previous: " ++ toString prev ++ ")"

            Nothing ->
                text ""
        ]

formatPaths : Maybe (List Key) -> Html Msg
formatPaths list =
    case list of
        Nothing -> div [][]
        Just paths -> div [] <| List.map formatKey paths

formatKey : Key -> Html Msg
formatKey key =
    li [] [ text <| toString key ]