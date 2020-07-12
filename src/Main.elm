module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, placeholder, value)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Measurement = Maybe Float

type alias UnderMeasurements = { loose : Measurement
                               , comfy_snug : Measurement
                               , tight : Measurement
                               }

type alias BustMeasurements = { standing: Measurement
                              , leaning_forward : Measurement
                              , lying_down : Measurement
                              }

type alias Model = { under_measurements : UnderMeasurements
                   , bust_measurements : BustMeasurements
                   }

init : Model
init = { under_measurements = { loose = Nothing
                              , comfy_snug = Nothing
                              , tight = Nothing
                              }
       , bust_measurements = { standing = Nothing
                             , leaning_forward = Nothing
                             , lying_down = Nothing
                             }
       }


type Msg
  = Loose String
  | Comfy String
  | Tight String
  | Standing String
  | Forward String
  | Backward String

update : Msg -> Model -> Model
update msg model =
  let
    under = model.under_measurements
    bust = model.bust_measurements
  in
    case msg of
      Loose val -> { model | under_measurements = { under | loose = String.toFloat val } }
      Comfy val -> { model | under_measurements = { under | comfy_snug = String.toFloat val } }
      Tight val -> { model | under_measurements = { under | tight = String.toFloat val } }
      Standing val -> { model | bust_measurements = { bust | standing = String.toFloat val } }
      Forward val -> { model |  bust_measurements = { bust | leaning_forward = String.toFloat val } }
      Backward val -> { model | bust_measurements = { bust | lying_down = String.toFloat val } }

view : Model -> Html Msg
view model =
  div []
    [ measurementInput model.under_measurements.loose " Loose fit"
    , measurementInput model.under_measurements.comfy_snug " Comfy snug"
    , measurementInput model.under_measurements.tight " Tight fit"
    , measurementInput model.bust_measurements.standing " Standing"
    , measurementInput model.bust_measurements.leaning_forward " Leaning forward"
    , measurementInput model.bust_measurements.lying_down " Lying down"
    ]

measurementInput : Measurement -> String -> Html Msg
measurementInput measurement label =
  div []
    [ input [ placeholder "0", type_ "input", value (measurementAsString measurement) ] []
    , text label
    ]

measurementAsString : Measurement -> String
measurementAsString measurement =
  case measurement of
    Just value -> String.fromFloat value
    Nothing -> ""
