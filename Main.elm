module Main where

import Signal exposing (..)
import Effects exposing (..)
import StartApp exposing (App)
import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, circle, rect)
import Svg.Attributes exposing (cx,cy,r,x,y,width,height,fill)
import Task exposing (..)
import Time exposing (..)
import Keyboard

type alias Model =
  {time : Maybe Float
  ,alive : Bool
  ,jumpState : Maybe Float
  ,obstacles : List Float
  ,score : Float}

type Action
  = Tick Float
  | Jump

initialModel : Model
initialModel =
  {time = Nothing
  ,alive = True
  ,jumpState = Nothing
  ,obstacles = [800, 1400]
  ,score = 0.0}

initialEffects : Effects Action
initialEffects =
  none

------------------------------------------------------------

obstacleView : Float -> Svg
obstacleView n =
  Svg.rect [x (toString n)
           ,y (toString obstacleTop)
           ,width (toString obstacleWidth)
           ,height (toString obstacleHeight)
           ,fill "cyan"]
           []

heroRadius : Float
heroRadius = 50

heroX : Float
heroX = 200

heroSpeed : Float
heroSpeed = 0.002

obstacleWidth : Float
obstacleWidth = 100

obstacleHeight : Float
obstacleHeight = 100

obstacleTop : Float
obstacleTop = 350

obstacleSpeed : Float
obstacleSpeed = 0.5

rootView : Address Action -> Model -> Html
rootView _ model =
  div []
      [div [] [code [] [text (toString model)]]
      ,h1 [style [("font-family", "monospace")]]
          [text ("Score: " ++ (toString (floor model.score)))]
      ,Svg.svg [width "800px"
               ,height "500px"]
               ((List.map obstacleView model.obstacles)
                ++
               [circle [cx (toString heroX)
                        ,cy (toString (jumpOffset (Maybe.withDefault 0 model.jumpState)))
                        ,r (toString heroRadius)
                        ,fill (if model.alive
                              then "black"
                              else "red")] []]
                )]

stepObstacle : Float -> Float -> Float
stepObstacle tick n =
  let new = (n - (tick * obstacleSpeed))
  in if new < 0 - obstacleWidth
     then 1000
     else new

crash : Maybe Float -> Float -> Bool
crash jumpState obstacleX =
  let cx = heroX
      cy = (jumpOffset (Maybe.withDefault 0 jumpState))
      bx1 = obstacleX
      bx2 = obstacleX + obstacleWidth
      by = obstacleTop + (obstacleHeight / 2)
      r = heroRadius
  in (cy + r) > by
     &&
     ((cx + r > bx1)
     &&
     (cx - r < bx2))

stepHero : Float -> Float -> Float
stepHero tick state =
  (tick * heroSpeed) + state

update : Action -> Model -> Model
update action model =
  case action of
    Tick n -> let newAlive = not (List.any (crash model.jumpState) model.obstacles)
              in if newAlive
                 then {model | time <- Just n
                             , score <- model.score + n
                             , jumpState <- let jumpState' = Maybe.map (stepHero n) model.jumpState
                                            in if (Maybe.withDefault 1 jumpState') < 1
                                               then jumpState'
                                               else Nothing
                             , obstacles <- List.map (stepObstacle n) model.obstacles
                             , alive <- newAlive}
                 else { model | alive <- newAlive}
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
                     ,inputs = [Signal.map Tick (Time.fps 40)
                               ,Signal.map (always Jump) Keyboard.space]}

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
