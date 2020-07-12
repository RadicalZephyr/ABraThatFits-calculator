module Main exposing (..)

import Browser
import Html exposing (Html, div, text)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = {}

init : Model
init = {}

type Msg
  = Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    Msg ->  model

view : Model -> Html Msg
view model =
  div []
    [text "Hello Sandbox!"]
