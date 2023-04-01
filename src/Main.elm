module Main exposing (main, Model, GameState)

import Html exposing (Html, div, p, text, button)
import Html.Attributes as Attrs 
import Html.Events exposing (onClick)
import Set exposing (empty, insert, member)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import Debug exposing (toString)
import Browser
import Browser.Events exposing (onKeyDown)
import Json.Decode as Json
import Process
import Task
import Random

import Tablero

import Gem 
import Time
import Matrix
import Array exposing (length)
import Dict


type alias Stats = 
    {
        killedEnemies: Int
        , maxProgress: Float
    }


type alias Model =
    {   
        points: Int
      , stepTime : Float

      , pausedState : GameState
      , state: GameState
      , isBuild : Bool

      , modelTablero : Tablero.Model
    }


type GameState = Paused | Playing | NotStarted | Finished

type Msg =
      HandleKeyboardEvent KeyboardEvent
    -- handle startup
    | Advance
    | Pause Bool
    | None
    | TableroMsg Tablero.Msg 


init : ( Model, Cmd Msg )
init =
    ( {   points = 0
        , modelTablero = Tablero.init 37 37
        , pausedState = NotStarted
        , state = NotStarted
        , stepTime = 1000.0
        , isBuild = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            if model.state == Playing then
                case event.keyCode of
                    Key.Escape -> 
                        ( {model | state = Paused, pausedState = model.state},  Cmd.none)     

                            
                    _ -> (model, Cmd.none)
                            
            else
                case event.keyCode of
                    Key.Escape -> ( {model | state = if model.state == Paused then Playing else model.state },  Cmd.none)
                    Key.Enter ->  (model, Cmd.none)
                    _ -> (model, Cmd.none)

        Pause paused -> ( {model | state = if paused then Paused else model.pausedState
                                , pausedState = model.state}, Cmd.none)

        Advance -> (model, Cmd.none)
        None -> (model, Cmd.none)

        TableroMsg childMsg ->
            let
                (newModelTablero, tableroCmd) =  Tablero.update childMsg model.modelTablero
            in
                ({ model | modelTablero = newModelTablero}, Cmd.map (\cmd -> TableroMsg cmd) tableroCmd)
        


             
issueMsgAsCmd : Msg -> Cmd Msg
issueMsgAsCmd msg =
    Task.perform identity (Task.succeed msg)

        
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


view : Model -> Html Msg
view model =
    Html.main_
        [Attrs.id "gemtd"]
        [ viewStartButton model.state
        , viewPauseButton model.state
        , Tablero.view model.isBuild model.modelTablero |> Html.map TableroMsg
        ]


viewStartButton : GameState -> Html Msg
viewStartButton state = 
    case state of
        NotStarted -> button [ Attrs.class "t-start"] [ text "Start" ]
        Finished -> button [ Attrs.class "t-start"] [ text "Start" ]
        _ -> button [ Attrs.disabled True, Attrs.hidden True] [ text "Start" ]


viewPauseButton : GameState -> Html Msg
viewPauseButton state = 
    case state of
        Paused -> button [onClick <| Pause False, Attrs.class "t-resume"] [ text "Resume" ]
        Playing -> button [onClick <| Pause True, Attrs.class "t-pause"] [ text "Pause" ]
        _ ->  button [ Attrs.disabled True, Attrs.hidden True] [ text "Pause" ]


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        , Time.every model.stepTime (\_ -> Advance)]

main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
        