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
import Browser.Events
import Dict exposing (Dict)


type alias AnimationsMap = Animator.Timeline (Dict Int (Enemy, Animator.Timeline AliveEnemy))

type alias Model = 
    {
          paths: List Path
        , alive: AnimationsMap
        , wave: Int
        , passed: List Enemy
        , updateFreq : Int
    }

type Msg = 
      Tick Time.Posix
    | Frame Float
    | StartWave
    | UpdatePaths (List Path)
    | NewWabe (List Enemy)

type AliveEnemy 
    = Pig Action Direction

type Action
    = Walking

type Direction
    = Left
    | Right

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

subscriptions : Sub Msg
subscriptions  =
    Sub.batch 
        [  Browser.Events.onAnimationFrame Tick 
            , Browser.Events.onAnimationFrameDelta Frame
        ]


animator : Animator.Animator Model
animator  = 
    Animator.animator
        |> Animator.watchingWith .alive
            (\newAlive model ->
                { model | alive = newAlive }
            )
            (\newAlive ->
                (Dict.size newAlive ) > 0
            )

    -- Animator.animator 
    --     |> Animator.watchingWith  .model (\t m ->  )


init : Model
init = {
          paths = [[]]
        , alive = Animator.init (Dict.empty)
        , wave = 0
        , passed = []
        , updateFreq = 100
    }


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of 
        
        StartWave -> ( {model | wave = model.wave + 1}, (startWave <| model.wave + 1))
        NewWabe wave -> ({model | alive = (initTimeLines wave)}, Cmd.none )
        UpdatePaths paths -> ({model | paths = paths}, Cmd.none )
        Tick newTime -> ( { model | alive =  Animator.updateTimeline newTime model.alive}, Cmd.none)
        Frame frame -> ( {model | alive = moveEnemies model.paths frame model.alive } , Cmd.none )


initFrenziedPig : Enemy 
initFrenziedPig = 
    {
          id = 0
        , name  = "Frenzied Pig"
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

initTimeLines :  List Enemy -> AnimationsMap
initTimeLines enemies =
    Animator.init <|
        List.foldl 
        (\e map -> Dict.insert e.id (e, Animator.init  (Pig Walking Right) ) map)
        Dict.empty
        enemies


moveEnemies : List Path -> Float -> AnimationsMap -> AnimationsMap
moveEnemies paths dt map =
    map

walk : (Int, Int) -> Enemy -> Enemy
walk (dx, dy) enemy =
    let
        (ex, ey) = enemy.position
    in
        { enemy | position = (ex + toFloat dx, ey + toFloat dy)}


startWave : Int -> Cmd Msg
startWave level = 
    Random.generate (\wave -> NewWabe wave) 
        <| waveGenerator level


enemyGenerator : Int -> Random.Generator Enemy
enemyGenerator id = 
       Random.andThen (\fp -> Random.map (\isElite -> { fp | isElite = isElite }) <| Random.weighted (1, True) [ (99, False)])
    <| Random.andThen (\dmg -> Random.constant { initFrenziedPig | damage = dmg, id = id})
    <| Random.int 0 19
        
        
waveGenerator : Int -> Random.Generator (List Enemy)
waveGenerator count =
     Random.andThen (\l ->  
        generateEnemies <| List.map enemyGenerator l
     )
    <| Random.constant (List.range 1 10)


generateEnemies : List (Random.Generator Enemy) -> Random.Generator (List Enemy)
generateEnemies generators =
    case generators of
        [] ->
            Random.constant []

        head :: tail ->
            Random.map2 (::) head (generateEnemies tail)


view : Model -> Html Msg
view model = 
    Html.div [Attrs.class "g-e-wave"]
       <| List.map Tuple.second
       <| Dict.toList 
       <| Dict.map (\k (e, tl) -> enemyView e tl)
       <| Animator.arrived model.alive


enemyView : Enemy -> Animator.Timeline AliveEnemy -> Html Msg
enemyView enemy timeline = 
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
                      
            --     [ Html.text enemy.name
            --     , Html.text <| Debug.toString enemy.isElite
            --     , Html.text <| "asas" ++ (Debug.toString x)
            --     , Html.text <| Debug.toString y
            -- ]
             [ viewSprite
                    (Animator.step timeline <|
                        \(Pig action direction) ->
                            let
                                -- this is where we decide to show the left or the right sprite.
                                --
                                frame mySprite =
                                    case direction of
                                        Left ->
                                            Animator.frame mySprite

                                        Right ->
                                            Animator.frame { mySprite | flipX = True }
                            in
                            case action of

                                Walking ->
                                    -- when we're in a `Walking` state, we want to cycle through 3 frames.
                                    -- And we can also specify our frames per secton
                                    Animator.framesWith
                                        -- `transition` are the frames we'd want to take when transitioning to this state.
                                        { transition = frame sprite.tail.stand

                                        -- `resting` is what we want to do while we're in this state.
                                        , resting =
                                            Animator.cycle
                                                (Animator.fps 15)
                                                [ frame sprite.tail.step1
                                                , frame sprite.tail.step2
                                                , frame sprite.tail.stand
                                                ]
                                        }

                    )
                ]
        else
            Html.div [] []




viewSprite : Box -> Html msg
viewSprite box =
    Html.div []
        [ Html.div
            [ Attrs.style "position" "absolute"
            , Attrs.style "top" (String.fromInt box.adjustY ++ "px")
            , Attrs.style "left" (String.fromInt box.adjustX ++ "px")
            , Attrs.style "width" (String.fromInt box.width ++ "px")
            , Attrs.style "height" (String.fromInt box.height ++ "px")
            , Attrs.style "background-image" "url('http://mdgriffith.github.io/elm-animator/images/mario-sprites.png')"
            , Attrs.style "background-repeat" "no-repeat"
            , Attrs.style "transform-origin" "30% 50%"
            , Attrs.style "transform"
                (if box.flipX then
                    "scaleX(-1) scale(2)"

                 else
                    "scaleX(1) scale(2)"
                )
            , Attrs.style "background-position"
                ("-"
                    ++ (String.fromInt box.x ++ "px -")
                    ++ (String.fromInt box.y ++ "px")
                )

            -- we need to tell the browser to render our image and leave the pixels pixelated.
            , Attrs.class "pixel-art"
            ]
            []
        ]



type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , adjustX : Int
    , adjustY : Int
    , flipX : Bool
    , flipY : Bool
    }


sprite =
    { tail =
        { stand =
            { x = 0
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step1 =
            { x = 30
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step2 =
            { x = 60
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        }
    }
