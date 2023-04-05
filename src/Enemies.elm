module Enemies exposing (..)

import Html exposing (Html)
import Html.Attributes as Attrs 

import TableroTypes exposing (..)
import Abilities exposing (Ability)
import Random
import Char exposing (isLower)
import Time
import Debug
import Animator


type alias Model = 
    {
          paths: List Path
        , alive: List Enemy
        , aliveAnimations : List (Animator.Timeline Enemy)
        , wave: Int
        , passed: List Enemy
        , updateFreq : Int
    }

type Msg = NoOp
    | StartWave
    | UpdatePaths (List Path)
    | NewWabe (List Enemy)
    | Move 

type alias Enemy = 
    {
          name : String
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

subscriptions :  Sub Msg
subscriptions =
    -- Time.every 100 (\_ -> Cmd.map (\m -> Move) a)
    Time.every 1000 (\_ -> Move)


init : Model
init = {
          paths = [[]]
        , alive = []
        ,aliveAnimations = []
        , wave = 0
        , passed = []
        , updateFreq = 100
    }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of 
        NoOp -> (model, Cmd.none) 
        StartWave -> ( {model | wave = model.wave + 1}, (startWave <| model.wave + 1))
        NewWabe wave -> ({model | alive = wave}, Cmd.none )
        Move -> ({model | alive = moveEnemies model.paths model.alive }, Cmd.none )
        UpdatePaths paths -> ({model | paths = paths}, Cmd.none )

moveEnemies : List Path -> List Enemy -> List Enemy
moveEnemies paths enemies = 
    List.map (\enemy -> moveEnemy paths enemy) enemies 
        

moveEnemy : List Path -> Enemy -> Enemy
moveEnemy p e =
    let 
        testPath = \en path -> 
            let 
                (ecf, futuref, onNextf) = List.foldl 
                    (\(px, py) (ec, future, onNext) ->
                        if future then 
                            (ec, future, False)
                        else
                            let 
                                (ex, ey) = e.position
                            in
                                if (ex == px && ey == py) then
                                    (ec, future, True)
                                else
                                    if onNext then
                                        ({ec | position = (px, py)}, True, False)
                                    else
                                        (ec, False, False)
                        )
                    (en, False, False)
                    path
            in 
                (ecf, futuref)

        (enemy, found) = testPath e
            <| List.map (\(xp ,yp) -> (Basics.toFloat xp, Basics.toFloat yp))
            <| List.concat p 

        (xe, ye) = enemy.position
    in  
        {enemy | position = (xe + 1, ye + 1) }
        

initFrenziedPig : Enemy 
initFrenziedPig = 
    {
          name  = "Frenzied Pig"
        , lvl = 1
        , health = 3
        , movement = 446
        , armor = 0
        , magicResistance  = 10
        , abilitites = []
        , damage = 0
        , isElite = False
        , position = (4, 4)
        , origin = (178, 60)
    }


startWave : Int -> Cmd Msg
startWave level = 
    Random.generate (\wave -> NewWabe wave) 
        <| waveGenerator level


enemyGenerator : Random.Generator Enemy
enemyGenerator = 
       Random.andThen (\fp -> Random.map (\isElite -> { fp | isElite = isElite }) <| Random.weighted (1, True) [ (99, False)])
    <| Random.andThen (\dmg -> Random.constant { initFrenziedPig | damage = dmg})
    <| Random.int 0 19
        
        
waveGenerator : Int -> Random.Generator (List Enemy)
waveGenerator count =
    Random.list 10 enemyGenerator



view : Model -> Html Msg
view model = 
    Html.div [Attrs.class "g-e-wave"]
       <| List.map (\e -> enemyView e) model.alive
    

enemyView : Enemy -> Html Msg
enemyView enemy = 
    let 
        (x, y) = enemy.position
        -- translate(tx, ty)
        visible = (\xi yi -> True)
        
        transalation = String.join ""
            [ "translate("
            , (Debug.toString  <| x * 30 )
            , "px, "
            , (Debug.toString <| y * 30)
            , "px)"
            ]

        -- trna
    in
        if visible x y then
            Html.div [Attrs.class  "g-e-enemy"
                      ,Attrs.style "top" <| (Debug.toString <| Tuple.first enemy.origin) ++ "px"
                      ,Attrs.style "left"  <| (Debug.toString <| Tuple.second enemy.origin) ++ "px"
                      , Attrs.style "transform" transalation ] 
                      
                [ Html.text enemy.name
                , Html.text <| Debug.toString enemy.isElite
                , Html.text <| "asas" ++ (Debug.toString x)
                , Html.text <| Debug.toString y
            ]
        else
            Html.div [] []

