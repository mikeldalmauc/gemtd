module Enemy exposing (..)


import Abilities exposing (Ability)

type alias Enemy = 
    {
          id : Int
        , name : String
        , lvl : Int
        , health : Int
        , movement : Int
        , armor : Int
        , magicResistance : Int
        , abilitites : List Ability
        , damage : Int
        , isElite : Bool
        , position : (Float, Float)
        , origin : (Int, Int)
    }
