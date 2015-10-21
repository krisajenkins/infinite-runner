module Main where

import Signal exposing (..)
import Effects exposing (..)
import StartApp exposing (App)
import Html exposing (..)
import Svg exposing (Svg, svg, circle, rect)
import Svg.Attributes exposing (..)
import Task exposing (..)
import Time exposing (..)
import Keyboard

type alias Model =
  {time : Maybe Float
  ,jumpState : Maybe Float
  ,obstacles : List Float}

type Action
  = Tick Float
  | Jump

initialModel : Model
initialModel =
  {time = Nothing
  ,jumpState = Nothing
  ,obstacles = [800, 1400]}

initialEffects : Effects Action
initialEffects =
  none

------------------------------------------------------------

obstacleView : Float -> Svg
obstacleView n =
  Svg.rect [x (toString n)
           ,y (toString 350)
           ,width "100"
           ,height "100"
           ,fill "cyan"]
           []

rootView : Address Action -> Model -> Html
rootView _ model =
  div []
      [div [] [code [] [text (toString model)]]
      ,Svg.svg [width "800px"
               ,height "500px"]
               ((circle [cx "100px"
                        ,cy (toString (jumpOffset (Maybe.withDefault 0 model.jumpState)))
                        ,r "50px"] [])
                ::
                (List.map obstacleView model.obstacles))]

stepObstacle : Float -> Float
stepObstacle n =
  let new = (n - 20)
  in if new < 0
     then 1000
     else new

update : Action -> Model -> Model
update action model =
  case action of
    Tick n -> {model | time <- Just n
                     , jumpState <- let jumpState' = Maybe.map ((+) (n / 1000)) model.jumpState
                                    in if (Maybe.withDefault 1 jumpState') < 1
                                       then jumpState'
                                       else Nothing
                     , obstacles <- List.map stepObstacle model.obstacles}
    Jump -> (case model.jumpState of
              Nothing -> {model | jumpState <- Just 0}
              _ -> model)

effect : Action -> Model -> Effects Action
effect action model = none

jumpOffset : Float -> Float
jumpOffset x = 400 * (4 * ((x - 0.5) ^2))

------------------------------------------------------------
-- App Startup
------------------------------------------------------------

app : App Model
app = StartApp.start {init = (initialModel
                             ,initialEffects)
                     ,view = rootView
                     ,update = \action model -> let newModel = update action model
                                                    newEffects = effect action newModel
                                                in (newModel, newEffects)
                     ,inputs = [Signal.map Tick (Time.fps 20)
                               ,Signal.map (always Jump) Keyboard.space]}

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
