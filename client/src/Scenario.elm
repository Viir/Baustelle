module Scenario exposing (Model, JointId, FromPlayerMsg (..), progress, updateForPlayerInputs)

import Base exposing (..)
import Vector2 exposing (Float2)
import Dict

type alias JointId = Int

type alias Model =
  {
    joints : Dict.Dict JointId Joint,
    components : Dict.Dict (JointId, JointId) Component,
    permSupport : Dict.Dict JointId Float2,
    tempSupport : Dict.Dict JointId Float2
  }

type alias Joint =
  {
    location : Float2,
    velocity : Float2
  }

type alias Component =
  {
    builtLength : Float
  }

type FromPlayerMsg
  = BuildComponent JointId JointId
  | TempSupportForJoint JointId Float2

type alias ScenarioConfig =
  {
    gravitation : Float2,
    maintainComponentLengthForceFactor : Float,
    dampFactor : Float
  }

config : ScenarioConfig
config =
  {
    gravitation = (0, 1e-4),
    maintainComponentLengthForceFactor = 3e-2,
    dampFactor = 1e-3
  }

updateStepDuration : Int
updateStepDuration = 10

progress : Int -> Model -> Model
progress duration scenario =
  let
    fullStepCount = duration // updateStepDuration
    remainderStepDuration = duration - (fullStepCount * updateStepDuration)
    listStepDuration = remainderStepDuration :: (updateStepDuration |> List.repeat fullStepCount)
  in
    scenario |> withListTransformApplied (listStepDuration |> List.map updateStep)

updateStep : Int -> Model -> Model
updateStep duration scenario =
  let
    durationFloat = duration |> toFloat

    gravitationAcceleration = config.gravitation |> Vector2.scale durationFloat

    jointsFromSupport : Dict.Dict JointId Joint
    jointsFromSupport =
      scenario.permSupport |> Dict.union scenario.tempSupport
      |> Dict.map (\_ location -> { location = location, velocity = (0, 0)})

    joints =
      scenario.joints |> Dict.union jointsFromSupport
      |> Dict.map (\jointId joint ->
        let
          connectedComponentsForce =
            scenario.components |> Dict.map (\(joint0Id, joint1Id) component ->
              if (joint0Id == jointId || joint1Id == jointId) |> not
              then (0, 0)
              else
                let
                  otherJointId = if joint0Id == jointId then joint1Id else joint0Id
                in
                  case scenario.joints |> Dict.get otherJointId of
                  Nothing -> (0, 0)
                  Just otherJoint ->
                    let
                      componentLength = joint.location |> Vector2.distance otherJoint.location
                      expansionForce = component.builtLength / componentLength - 1
                      forceToMaintainLength = Vector2.directionFromTo otherJoint.location joint.location |> Vector2.scale (expansionForce * 1e-2)
                    in
                      forceToMaintainLength)

          connectedComponentsForceSum =
            connectedComponentsForce |> Dict.values |> List.foldl (\c0 c1 -> Vector2.add c0 c1) (0, 0)
          
          combinedAcceleration = gravitationAcceleration |> Vector2.add connectedComponentsForceSum

          dampFactor = (1 + config.dampFactor) ^ durationFloat

          velocity =
            joint.velocity |> Vector2.add combinedAcceleration |> Vector2.scale (1 / dampFactor)

          jointLocation = joint.location |> Vector2.add (joint.velocity |> Vector2.scale durationFloat)
        in
          { joint | location = jointLocation, velocity = velocity })
  in
    { scenario | joints = joints }

updateForPlayerInputs : List FromPlayerMsg -> Model -> Model
updateForPlayerInputs listFromPlayerInput scenario =
  scenario |> withListTransformApplied (listFromPlayerInput |> List.map updateForPlayerInput)

updateForPlayerInput : FromPlayerMsg -> Model -> Model
updateForPlayerInput msg scenario =
  case msg of
  BuildComponent startJointId endJointId ->
    if startJointId == endJointId
    then scenario
    else
      case distanceFromJointsInScenario scenario (startJointId, endJointId) of
      Just length ->
        let
          component = { builtLength = length }
        in
          { scenario | components = scenario.components |> Dict.insert (startJointId, endJointId) component }
      _ -> scenario
  TempSupportForJoint jointId supportLocation ->
    if scenario.joints |> Dict.member jointId
    then scenario -- We do not support changing location of existing joints using this imput.
    else
      let
        tempSupport = Dict.singleton jointId supportLocation -- Only one temp support is allowed for a given point in time.
      in
        { scenario | tempSupport = tempSupport }
    |> progress 0

distanceFromJointsInScenario : Model -> (JointId, JointId) -> Maybe Float
distanceFromJointsInScenario scenario (joint0Id, joint1Id) =
  case (scenario.joints |> Dict.get joint0Id, scenario.joints |> Dict.get joint1Id) of
  (Just joint0, Just joint1) -> Just (joint0.location |> Vector2.distance joint1.location)
  _ -> Nothing
