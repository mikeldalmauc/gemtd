module TableroTypes exposing (..)
import Gem exposing (Gem)

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
    | Flag


type alias AdjacencyList =  List  (Int, List Int)


type alias Path =  List (Int, Int)

type alias Vertex = {
       vertexDistance : Maybe Int
    , vertexPredecessor : Maybe Int
    }
