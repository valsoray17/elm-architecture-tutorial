import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { time : Time
  , paused : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model 0 False, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
  | Pause


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime} , Cmd.none)

    Pause ->
      ( { model | paused = not model.paused }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.paused then
    Sub.none
  else
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ drawClock model
    , button [ onClick Pause ] [ Html.text "Pause" ]
    ]


type alias HandStyle =
  { length : Float
  , strokeWidth : String
  }


drawClockHand : Float -> HandStyle -> Html Msg
drawClockHand angle handStyle =
  let
    handX =
      toString (50 + handStyle.length * cos (angle - degrees 90))

    handY =
      toString (50 + handStyle.length * sin (angle - degrees 90))
  in
    line [ x1 "50", y1 "50"
         , x2 handX, y2 handY
         , stroke "#023963", strokeWidth handStyle.strokeWidth ] []


drawClock : Model -> Html Msg
drawClock model =
  let
    secondAngle =
      degrees <| toFloat <| floor (Time.inSeconds model.time) % 60 * 6

    minuteAngle =
      degrees <| toFloat <| floor (Time.inMinutes model.time) % 60 * 6

    hourAngle =
      degrees <| toFloat <| floor (Time.inHours model.time) % 12 * 30
  in
    svg [ viewBox "0 0 100 100", width "300px" ]
              [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
              , drawClockHand secondAngle (HandStyle 40 "0.5px")
              , drawClockHand minuteAngle (HandStyle 30 "1px")
              , drawClockHand hourAngle (HandStyle 20 "1.5px")
              ]
