module Gem exposing (..)

import Random
import Debug exposing (toString)

type BasicGem = 
      Topaz Level
    | Opal Level
    | Emerald Level
    | Sapphire Level
    | Ruby Level
    | Diamond Level
    | Amethyst Level
    | Aquamarine Level

type AdvancedGem = 
      Malachite
    | Silver

type Gem
    = Advanced AdvancedGem
    | Basic BasicGem 

type alias Level = Int

gemGenerator : Int ->  Random.Generator Gem
gemGenerator heroLevel =
    let 
        weights = case heroLevel of 
            1 -> [100, 0, 0, 0, 0]
            2 -> [80, 20, 0, 0, 0]
            3 -> [60, 30, 10, 0, 0] -- REVISAR
            4 -> [30, 40, 20, 10, 0]
            5 -> [10, 30, 30, 20, 10]
            _ -> [0, 0, 0, 0, 100]

        -- Returns a pair of gem levl with a probability        
        mapper = \level prob-> 
            let
                l = level + 1
            in
                List.map (\gem ->  (toFloat prob / 8, gem) )
                    [ Topaz l
                    , Opal l
                    , Emerald l
                    , Sapphire l
                    , Ruby l
                    , Diamond l
                    , Amethyst l
                    , Aquamarine l
                    ]

        -- Taking the weights array and probability mapper
        -- generate all porbabilities, filter zeros
        probabilityTable = 
            List.indexedMap mapper weights
                |> List.concat
                |> List.filter (\(prob, _) -> prob > 0)

        (firstProb, rest) = case probabilityTable of
            [] -> ((100, Aquamarine 5), [])
            x :: xs -> (x, xs) 

    in
        Random.weighted firstProb rest
        |> Random.map (\g -> Basic g)  


gemName : Gem -> String
gemName pGem =
    case pGem of 
        Basic g-> basicGemName g
        Advanced g -> advancedGemName g


gemToString : Gem -> String
gemToString gem = 
    case gem of
        Advanced g -> (String.toLower (gemName gem)) 
        Basic g ->  (String.toLower (gemName gem))  ++ " " ++ (toString <| getLevel g)


cssClass : Gem -> String
cssClass gem =
    case gem of
        Advanced g -> "t-" ++ (String.toLower (gemName gem))
        Basic g -> "t-" ++ (toString <| getLevel g) ++ "-" ++ (String.toLower (gemName gem)) 

    

getLevel : BasicGem -> Int
getLevel g = 
    case g of 
        Topaz l -> l
        Opal  l -> l 
        Emerald l -> l 
        Sapphire l -> l 
        Ruby l -> l
        Diamond l -> l 
        Amethyst l -> l 
        Aquamarine l -> l 

advancedGemName : AdvancedGem -> String
advancedGemName gem = 
    case gem of
        Malachite ->
            "Malachite"
        
        Silver ->
            "Silver"


basicGemName : BasicGem -> String
basicGemName gem =  
    case gem of
        Topaz level ->
            "Topaz"
        
        Opal level ->
            "Opal"
        
        Emerald level ->
            "Emerald"
        
        Sapphire level ->
            "Sapphire"
        
        Ruby level ->
            "Ruby"
        
        Diamond level ->
            "Diamond"
        
        Amethyst level ->
            "Amethyst"
        
        Aquamarine level ->
            "Aquamarine"

