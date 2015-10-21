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

type alias Game =
  {time : Maybe Float
  ,alive : Bool
  ,jumpState : Maybe Float
  ,obstacles : List Float
  ,score : Float}

type alias Model =
  {currentGame : Game
  ,previousGames : List Game}

type Action
  = Tick Float
  | Jump
  | Rewind

initialModel : Model
initialModel =
  {currentGame = {time = Nothing
                 ,alive = True
                 ,jumpState = Nothing
                 ,obstacles = [800, 1400]
                 ,score = 0.0}
  ,previousGames = []}

------------------------------------------------------------
-- View
------------------------------------------------------------
rootView : Address Action -> Model -> Html
rootView _ model =
  div []
      [div [] [code [] [text (toString model.currentGame)]]
      ,h1 [style [("font-family", "monospace")]]
          [text ("Score: " ++ (toString (floor model.currentGame.score)))]
      ,h2 [] [text "Instructions"]
      ,p [] [text "<space> to jump, <shift> to jump back in time."]
      ,Svg.svg [width "800px"
               ,height "500px"]
               ((List.map obstacleView model.currentGame.obstacles)
                ++
               [circle [cx (toString heroX)
                       ,cy (toString (jumpOffset (Maybe.withDefault 0 model.currentGame.jumpState)))
                       ,r (toString heroRadius)
                       ,fill (if model.currentGame.alive
                             then "black"
                             else "red")] []])]

obstacleView : Float -> Svg
obstacleView n =
  Svg.rect [x (toString n)
           ,y (toString obstacleTop)
           ,width (toString obstacleWidth)
           ,height (toString obstacleHeight)
           ,fill "cyan"]
           []

------------------------------------------------------------
-- Computation
------------------------------------------------------------
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

stepHero : Float -> Float -> Float
stepHero tick state =
  (tick * heroSpeed) + state

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

------------------------------------------------------------
-- Loop
------------------------------------------------------------
updateGame : Action -> Game -> Game
updateGame action game =
  case action of
    Tick n -> let newAlive = not (List.any (crash game.jumpState) game.obstacles)
              in if newAlive
                 then {game | time <- Just n
                             , score <- game.score + n
                             , jumpState <- let jumpState' = Maybe.map (stepHero n) game.jumpState
                                            in if (Maybe.withDefault 1 jumpState') < 1
                                               then jumpState'
                                               else Nothing
                             , obstacles <- List.map (stepObstacle n) game.obstacles
                             , alive <- newAlive}
                 else { game | alive <- newAlive}
    Jump -> (case game.jumpState of
              Nothing -> {game | jumpState <- Just 0}
              _ -> game)
    Rewind -> game

update : Action -> Model -> Model
update action model =
  case action of
    Rewind -> let newGame = Maybe.withDefault model.currentGame (List.head (List.reverse model.previousGames))
             in {model | currentGame <- newGame
                       , previousGames <- []}
    _ -> let newGame = updateGame action model.currentGame
         in {model | currentGame <- newGame
                   , previousGames <- List.take 100 (model.currentGame :: model.previousGames)}


jumpOffset : Float -> Float
jumpOffset x = 400 * (4 * ((x - 0.5) ^2))

------------------------------------------------------------
-- App Startup
------------------------------------------------------------

app : App Model
app = StartApp.start {init = (initialModel, none)
                     ,view = rootView
                     ,update = \action model -> (update action model, none)
                     ,inputs = [Signal.map Tick (Time.fps 40)
                               ,Signal.map (always Jump) Keyboard.space
                               ,Signal.map (always Rewind) Keyboard.shift]}

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
