module Main where

import Signal exposing (..)
import Effects exposing (..)
import StartApp exposing (App)
import Html exposing (..)
import Task exposing (..)

type alias Model =
  {}

type Action =
  NoOp

initialModel : Model
initialModel = {}

initialEffects : Effects Action
initialEffects = none

rootView : Address Action -> Model -> Html
rootView _ _ = h1 [] [text "Hello"]

update : Action -> Model -> Model
update action model = model

effect : Action -> Model -> Effects Action
effect action model = none

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
                     ,inputs = []}

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
