module ScenarioConstruction exposing (..)
-- Functions for constructing a scenario/gameworld.

import Base exposing (..)
import GameConfig
import Scenario exposing (..)
import Dict
import Set
import Vector2 exposing (Float2)


emptyScenario : Scenario.Model
emptyScenario =
    {
        config = GameConfig.scenarioConfig,
        timeMilli = 0,
        joints = Dict.empty,
        permSupport = Dict.empty,
        tempSupport = Dict.empty,
        beams = Dict.empty,
        outsetJoints = Set.empty,
        adversaries = Dict.empty,
        supplies = 0
    }

jointIdsBase : Scenario.Model -> JointId
jointIdsBase scenario = ([ scenario.joints |> Dict.keys, scenario.permSupport |> Dict.keys ] |> List.concat |> List.maximum |> Maybe.withDefault 0) + 1

withPermSupportAddedAtLocations : List (Float2, Bool) -> Scenario.Model -> Scenario.Model
withPermSupportAddedAtLocations supportLocations scenario =
    let
      jointIdBase = jointIdsBase scenario

      permSupportAdditionWithOutsetFlag =
        supportLocations
        |> List.indexedMap (\i (supportLocation, isOutset) -> (jointIdBase + i, (supportLocation, isOutset)))
        |> Dict.fromList

      permSupportAddition = permSupportAdditionWithOutsetFlag |> dictMapValues Tuple.first
      outsetAddtion = permSupportAdditionWithOutsetFlag |> Dict.filter (\_ (_, isOutset) -> isOutset) |> Dict.keys |> Set.fromList
    in
      { scenario |
        permSupport = scenario.permSupport |> Dict.union permSupportAddition,
        outsetJoints = scenario.outsetJoints |> Set.union outsetAddtion  }

withJointsAddedAtLocations : List Float2 -> Scenario.Model -> Scenario.Model
withJointsAddedAtLocations locations scenario =
  let
    jointIdBase = jointIdsBase scenario

    jointsAddition = locations |> List.indexedMap (\i location -> (i + jointIdBase, { location = location, velocity = (0, 0)})) |> Dict.fromList
  in
    { scenario | joints = scenario.joints |> Dict.union jointsAddition}

withBeamsAddedForMaxLength : Float -> Scenario.Model -> Scenario.Model
withBeamsAddedForMaxLength maxLength scenarioBeforePropagationOfSupport =
  let
    scenario =
      { scenarioBeforePropagationOfSupport
      | joints = scenarioBeforePropagationOfSupport.joints |> Dict.union (Scenario.getJointsFromSupport scenarioBeforePropagationOfSupport) }

    allJointsDictIdAndLocation = [ scenario.permSupport, scenario.tempSupport, scenario.joints |> dictMapValues (\joint -> joint.location) ]

    allJointsIds = allJointsDictIdAndLocation |> List.map Dict.keys |> List.concat |> Set.fromList

    beamsBeforeConstraint : Dict.Dict (JointId, JointId) Scenario.Beam
    beamsBeforeConstraint =
      allJointsIds |> Set.toList
      |> List.map (\origJointId ->
        allJointsIds |> Set.toList |> List.filterMap (\destJointId ->
          case Scenario.distanceFromJointsInScenario scenario (origJointId, destJointId) of
          Nothing -> Nothing
          Just distance ->
            if origJointId < destJointId
            then Just ((origJointId, destJointId), { builtLength = distance })
            else Nothing))
      |> List.concat
      |> Dict.fromList

    beams =
      beamsBeforeConstraint
      |> Dict.filter (\_ beam -> beam.builtLength <= maxLength)
  in
    { scenario | beams = scenario.beams |> Dict.union beams }

