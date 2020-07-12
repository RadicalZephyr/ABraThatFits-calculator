module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (checked, disabled, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type Unit
 = Inches | Millimeters

type Measurement
 = Measurement (Maybe Float)

type alias BustMeasurements = { standing: Measurement
                              , leaning_forward : Measurement
                              , lying_down : Measurement
                              }

type alias UnderMeasurements = { loose : Measurement
                               , comfy_snug : Measurement
                               , tight : Measurement
                               }

type alias Model = { under_measurements : UnderMeasurements
                   , bust_measurements : BustMeasurements
                   , unit : Unit
                   , size : Maybe String
                   }

init : Model
init = { bust_measurements = { standing = Measurement Nothing
                             , leaning_forward = Measurement Nothing
                             , lying_down = Measurement Nothing
                             }
       , under_measurements = { loose = Measurement Nothing
                              , comfy_snug = Measurement Nothing
                              , tight = Measurement Nothing
                              }
       , unit = Inches
       , size = Nothing
       }

calculateSize : Model -> String
calculateSize model =
  ""

hasAllMeasurements : Model -> Bool
hasAllMeasurements model =
  underHasAllFields model.under_measurements && bustHasAllFields model.bust_measurements

bustHasAllFields : BustMeasurements -> Bool
bustHasAllFields bust_measurements =
  isJust bust_measurements.standing
    && isJust bust_measurements.leaning_forward
      &&  isJust bust_measurements.lying_down

underHasAllFields : UnderMeasurements -> Bool
underHasAllFields under_measurements =
  isJust under_measurements.loose
    && isJust under_measurements.comfy_snug
      &&  isJust under_measurements.tight


isJust : Measurement -> Bool
isJust (Measurement maybe) =
  case maybe of
    Just _ -> True
    Nothing -> False

type Msg
  = Calculate
  | SetUnit Unit
  | Loose Measurement
  | Comfy Measurement
  | Tight Measurement
  | Standing Measurement
  | Forward Measurement
  | Lying Measurement


update : Msg -> Model -> Model
update msg model =
  let
    under = model.under_measurements
    bust = model.bust_measurements
  in
    case msg of
      Calculate -> { model | size = Just (calculateSize model) }
      SetUnit unit -> { model | unit = unit }
      Loose val -> { model | under_measurements = { under | loose = val } }
      Comfy val -> { model | under_measurements = { under | comfy_snug = val } }
      Tight val -> { model | under_measurements = { under | tight = val } }
      Standing val -> { model | bust_measurements = { bust | standing = val } }
      Forward val -> { model |  bust_measurements = { bust | leaning_forward = val } }
      Lying val -> { model | bust_measurements = { bust | lying_down = val } }

view : Model -> Html Msg
view model =
  div []
    [ span [] [ unitCheckbox "Inches" Inches model.unit
              , unitCheckbox "Millimeters" Millimeters model.unit
              ]
    , measurementInput model.under_measurements.loose Loose " Loose fit"
    , measurementInput model.under_measurements.comfy_snug Comfy " Comfy snug"
    , measurementInput model.under_measurements.tight Tight " Tight fit"
    , measurementInput model.bust_measurements.standing Standing " Standing"
    , measurementInput model.bust_measurements.leaning_forward Forward " Leaning forward"
    , measurementInput model.bust_measurements.lying_down Lying " Lying down"
    , button [ onClick Calculate, disabled <| not <| hasAllMeasurements model ] [text "Calculate Size"]
    ]

unitCheckbox : String -> Unit -> Unit -> Html Msg
unitCheckbox label unit currentUnit =
  span []
    [ text label
    , input [ name "input-unit"
            , type_ "radio"
            , checked (currentUnit == unit)
            , onInput (\_ -> (SetUnit unit))
            ] []
    ]

measurementInput : Measurement -> (Measurement -> Msg) -> String -> Html Msg
measurementInput measurement msg label =
  div []
    [ input
        [ onInput (\v -> msg <| Measurement <| String.toFloat v)
        , placeholder "0.0"
        , type_ "input"
        , value (measurementAsString measurement)
        ]
        []
    , text label
    ]

measurementAsString : Measurement -> String
measurementAsString (Measurement measurement) =
  case measurement of
    Just value -> String.fromFloat value
    Nothing -> ""
