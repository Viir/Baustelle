module Game exposing (Model, Msg, progress, initialGame, update, view)

import Base exposing (..)
import Scenario
import ScenarioConstruction
import ScenarioViewport
import Random
import Dict
import Set
import Console
import Vector2 exposing (Float2)
import Svg
import Svg.Attributes as SA
import Html
import Html.Attributes exposing (style)
import Visuals exposing (HtmlStyle)


type alias Model =
    {
        scenario : Scenario.Model,
        toDefendJointId : Scenario.JointId,
        viewport : ScenarioViewport.Model
    }

type Msg = ScenarioViewport ScenarioViewport.Msg

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

initialGame : Model
initialGame = initialScenario |> gameFromScenario

gameFromScenario : Scenario.Model -> Model
gameFromScenario scenario =
    let
        -- pick the highest joint as the joint to defend.
        toDefendJointId =
            scenario.joints |> Dict.toList
            |> List.sortBy (\(_, joint) -> joint.location |> Tuple.second) |> List.reverse
            |> List.map Tuple.first
            |> List.head |> Maybe.withDefault -9999
    in
        { scenario = scenario, toDefendJointId = toDefendJointId, viewport = ScenarioViewport.defaultViewport }

adversaryAdditionAverageDistance : Int
adversaryAdditionAverageDistance = 4000

progress : Int -> Random.Seed -> Model -> Model
progress duration randomSeed game =
    if game |> isGameOver then game else
    let
        addAdversary = (Random.step (Random.int 0 adversaryAdditionAverageDistance) randomSeed |> Tuple.first) < duration

        scenario =
            if addAdversary
            then game.scenario |> withAdversaryAddedAtRandomLocation randomSeed
            else game.scenario
    in
        { game | scenario = (scenario |> Scenario.progress duration) }

withAdversaryAddedAtRandomLocation : Random.Seed -> Scenario.Model -> Scenario.Model
withAdversaryAddedAtRandomLocation seed scenario =
    let
        jointsToAvoidIds = scenario.permSupport |> Dict.keys |> Set.fromList
        locationsToChooseFrom =
            scenario.beams |> Dict.keys |> List.filter (\(joint0, joint1) -> [ joint0, joint1 ] |> List.all (\joint -> jointsToAvoidIds |> Set.member joint) |> not)
    in
        case locationsToChooseFrom |> listRandomItem seed of
        Nothing -> scenario
        Just adversaryLocation -> scenario |> Scenario.withAdversaryAddedOnBeam (adversaryLocation, 1)

update : Msg -> Model -> Model
update msg game =
    if game |> isGameOver then game else
    case msg of
    ScenarioViewport viewportMsg ->
        let
            (viewport, listToGameInput) = (ScenarioViewport.update viewportMsg game.scenario game.viewport)
            scenario = (game.scenario |> Scenario.updateForPlayerInputs listToGameInput)
        in
            { game | scenario = scenario, viewport = viewport }

view : Model -> Html.Html Msg
view gameBeforeUpdate =
    let
        game = { gameBeforeUpdate | scenario = gameBeforeUpdate.scenario |> Scenario.progress 0 }

        resultFromMouseEventAtCurrentLocation = getResultFromMouseEventAtCurrentLocation game

        scenarioViewModel =
            {
                scenario = game.scenario,
                scenarioAfterMouseUpEvent = (resultFromMouseEventAtCurrentLocation Console.MouseUp).scenario,
                viewportAfterMouseDownEvent = (resultFromMouseEventAtCurrentLocation Console.MouseDown).viewport
            }

        (viewportWidth, viewportHeight) = ScenarioViewport.viewportSize

        scenarioView = ScenarioViewport.view scenarioViewModel |> Html.map ScenarioViewport

        svgContent =
            if game |> isGameOver
            then
                [
                    scenarioView |> List.singleton |> Svg.g [ style [("opacity","0.7")]],
                    gameOverView game
                ] |> Svg.g []
            else scenarioView
    in
        [ svgContent ]
        |> Svg.svg [ SA.width (viewportWidth |> toString), SA.height (viewportHeight |> toString), style viewportStyle ]

gameOverView : Model -> Html.Html a
gameOverView game =
    Svg.text_ [ style gameOverTextStyle ] [ Svg.text "GAME OVER" ]
    |> List.singleton |> Visuals.svgGroupWithTranslationAndElements ((ScenarioViewport.viewportSize |> Tuple.first) / 2, 100)

updateForPlayerInputs : List Scenario.FromPlayerMsg -> Model -> Model
updateForPlayerInputs listFromPlayerInput game =
    { game | scenario = (game.scenario |> Scenario.updateForPlayerInputs listFromPlayerInput)}

isGameOver : Model -> Bool
isGameOver game =
    game.scenario.joints |> Dict.get game.toDefendJointId
    |> Maybe.andThen (\toDefendJoint -> (toDefendJoint.location |> Tuple.second) < 0 |> Just)
    |> Maybe.withDefault True

getResultFromMouseEventAtCurrentLocation : Model -> Console.MouseEventType -> Model
getResultFromMouseEventAtCurrentLocation game eventType =
    case game.viewport.mouseLocationInWorld of
    Nothing -> game
    Just mouseLocationInWorld ->
        game |> update ((getMouseLeftButtonEventForOffset eventType mouseLocationInWorld) |> ScenarioViewport.MouseEvent |> ScenarioViewport)

getMouseLeftButtonEventForOffset : Console.MouseEventType -> Float2 -> Console.MouseEvent
getMouseLeftButtonEventForOffset eventType offset =
  {
    button = Console.Left,
    eventType = eventType,
    offset = offset,
    wheelDelta = (0,0)
  }

viewportStyle : HtmlStyle
viewportStyle =
    [
        ("background", "#101010"),
        ("cursor","default"),
        ("user-select","none"),("-webkit-user-select","none"),("-moz-user-select","none"),("-ie-user-select","none")
    ]

gameOverTextStyle : HtmlStyle
gameOverTextStyle =
  [
    ("text-anchor","middle"),("font-size","60px"),("font-family","'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
    ("fill","whitesmoke"),("opacity","0.7")
  ]
