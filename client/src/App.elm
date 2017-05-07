import Html exposing (div, button, text)
import Game
import Time
import AnimationFrame
import Random


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
  = Game Game.Msg
  | TimeUpdate Time.Time

type alias Model =
  {
    timeMilli : Int,
    game : Game.Model
  }

init : (Model, Cmd Msg)
init =
  ({
    timeMilli = 0,
    game = Game.initialGame
  }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  AnimationFrame.times TimeUpdate

view : Model -> Html.Html Msg
view model =
  Game.view model.game
  |> Html.map Game

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  Game gameMsg -> ({ model | game = (Game.update gameMsg model.game)}, Cmd.none)
  TimeUpdate time ->
    let
      timeMilli = time |> round
      durationMilli = timeMilli - model.timeMilli
    in
      ({ model | timeMilli = timeMilli, game = Game.progress (durationMilli |> min 1000) (Random.initialSeed (timeMilli * 347161)) model.game }, Cmd.none)
