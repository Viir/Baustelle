import Html exposing (div, button, text)
import Scenario
import ScenarioViewport
import ScenarioConstruction
import Time


main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

type Msg
  = SiteViewport ScenarioViewport.Msg
  | TimeUpdate Time.Time

type alias Model =
  {
    timeMilli : Int,
    scenario : Scenario.Model,
    viewport : ScenarioViewport.Model
  }

init : (Model, Cmd Msg)
init =
  ({
    timeMilli = 0,
    scenario = initialScenario,
    viewport = ScenarioViewport.defaultViewport
  }, Cmd.none)

initialScenario : Scenario.Model
initialScenario =
  let
    jointDistanceHorizontal = 150

    rowFromJointCountAndHeight (jointCount, height) =
      List.range 0 (jointCount - 1) |> List.map (\i -> (i * jointDistanceHorizontal - ((jointCount - 1) * jointDistanceHorizontal // 2) |> toFloat, height))

    supportJointsLocations =
      rowFromJointCountAndHeight (4, 0)

    towerJointsLocations =
      [ (3, 130), (2, 260), (1, 450)] |> List.map rowFromJointCountAndHeight |> List.concat
  in
    ScenarioConstruction.emptyScenario
    |> ScenarioConstruction.withTempSupportRange 150
    |> ScenarioConstruction.withPermSupportAddedAtLocations
      (supportJointsLocations |> List.map (\location -> (location, True)))
    |> ScenarioConstruction.withJointsAddedAtLocations towerJointsLocations
    |> ScenarioConstruction.withBeamsAddedForMaxLength 240
    |> Scenario.progress 1000

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.second * 0.1) TimeUpdate

view : Model -> Html.Html Msg
view model =
  ScenarioViewport.view model.scenario model.viewport
  |> Html.map SiteViewport

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
  SiteViewport viewportMsg ->
    let
      (viewport, listToGameInput) = (ScenarioViewport.update viewportMsg model.scenario model.viewport)
      scenario = Scenario.updateForPlayerInputs listToGameInput model.scenario
    in
      ({ model | scenario = scenario, viewport = viewport }, Cmd.none)
  TimeUpdate time ->
    let
      timeMilli = time |> round
      durationMilli = timeMilli - model.timeMilli
    in
      ({ model | timeMilli = timeMilli, scenario = Scenario.progress (durationMilli |> min 1000) model.scenario }, Cmd.none)
