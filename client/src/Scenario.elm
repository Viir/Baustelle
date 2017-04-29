module Scenario exposing (Model, JointId, FromPlayerMsg (..), progress, updateForPlayerInputs, getAllReachedJointsIds, stressFactorFromComponent)

import Base exposing (..)
import Vector2 exposing (Float2)
import Dict
import Set


type alias JointId = Int

type alias Model =
  {
    joints : Dict.Dict JointId Joint,
    components : Dict.Dict (JointId, JointId) Component,
    permSupport : Dict.Dict JointId Float2,
    tempSupport : Dict.Dict JointId Float2,
    outsetJoints : Set.Set JointId,
    tempSupportRange : Float,
    maxHeightRecord : Float
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
    gravity : Float2,
    maintainComponentLengthForceFactor : Float,
    dampFactor : Float,
    componentFailThreshold : Float
  }

config : ScenarioConfig
config =
  {
    gravity = (0, -1e-4),
    maintainComponentLengthForceFactor = 3e-2,
    dampFactor = 3e-3,
    componentFailThreshold = 0.3
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

    gravityAcceleration = config.gravity |> Vector2.scale durationFloat

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
                      forceToMaintainLength =
                        Vector2.directionFromTo otherJoint.location joint.location
                        |> Vector2.scale (expansionForce * config.maintainComponentLengthForceFactor)
                    in
                      forceToMaintainLength)

          connectedComponentsForceSum =
            connectedComponentsForce |> Dict.values |> List.foldl (\c0 c1 -> Vector2.add c0 c1) (0, 0)

          combinedAcceleration = gravityAcceleration |> Vector2.add connectedComponentsForceSum

          dampFactor = (1 + config.dampFactor) ^ durationFloat

          velocity =
            joint.velocity |> Vector2.add combinedAcceleration |> Vector2.scale (1 / dampFactor)

          jointLocation = joint.location |> Vector2.add (joint.velocity |> Vector2.scale durationFloat)
        in
          { joint | location = jointLocation, velocity = velocity })

    afterMechanics =
      { scenario | joints = joints }
      |> removeJointsOutsideScenario |> updateForFailure

    maxHeightRecord = getCurrentBuildingHeight afterMechanics |> max afterMechanics.maxHeightRecord
  in
    { afterMechanics | maxHeightRecord = maxHeightRecord }

removeJointsOutsideScenario : Model -> Model
removeJointsOutsideScenario scenario =
  { scenario | joints = scenario.joints |> Dict.filter (\_ joint -> locationIsInsideScenario joint.location)}

updateForFailure : Model -> Model
updateForFailure scenario =
  let
    remainingComponents =
      scenario.components
      |> Dict.filter (\jointsIds component ->
        stressFactorFromComponent scenario jointsIds |> Maybe.andThen (\stressFactor -> Just (stressFactor < 1)) |> Maybe.withDefault False)

    remainingComponentsKeys = remainingComponents |> Dict.keys

    supportJointsIds = [ scenario.permSupport |> Dict.keys, scenario.tempSupport |> Dict.keys ] |> List.concat |> Set.fromList

    remainingJoints =
      scenario.joints
      |> Dict.filter (\jointId joint ->
        (supportJointsIds |> Set.member jointId) || (remainingComponentsKeys |> List.any (\(joint0Id, joint1Id) -> joint0Id == jointId || joint1Id == jointId)))
  in
    { scenario | components = remainingComponents, joints = remainingJoints }

updateForPlayerInputs : List FromPlayerMsg -> Model -> Model
updateForPlayerInputs listFromPlayerInput scenario =
  scenario |> withListTransformApplied (listFromPlayerInput |> List.map updateForPlayerInput)

updateForPlayerInput : FromPlayerMsg -> Model -> Model
updateForPlayerInput msg scenario =
  case msg of
  BuildComponent startJointId endJointId ->
    if startJointId == endJointId || (getAllReachedJointsIds scenario |> Set.member startJointId |> not)
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
        locationIsInRange =
          getAllReachedJointsIds scenario |> Set.toList
          |> List.filterMap (\jointId -> scenario.joints |> Dict.get jointId)
          |> List.any (\joint -> (joint.location |> Vector2.distance supportLocation) < scenario.tempSupportRange)
      in
        if not locationIsInRange
        then scenario
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

stressFactorFromComponent : Model -> (JointId, JointId) -> Maybe Float
stressFactorFromComponent scenario joints =
  case (distanceFromJointsInScenario scenario joints, scenario.components |> Dict.get joints) of
  (Just length, Just component) ->
    let
      stretchFactor = length / component.builtLength - 1
    in
      Just ((abs stretchFactor) / config.componentFailThreshold)
  _ -> Nothing

getAllReachedJointsIds : Model -> Set.Set JointId
getAllReachedJointsIds scenario =
  getAllReachedJointsIdsFromOutset (scenario.components |> Dict.keys) scenario.outsetJoints

getAllReachedJointsIdsFromOutset : List (JointId, JointId) -> Set.Set JointId -> Set.Set JointId
getAllReachedJointsIdsFromOutset connectedJoints outsetJointsIds =
  let
    nextStepReachableJointsIds =
      connectedJoints |> List.filterMap (\(joint0Id, joint1Id) ->
        if outsetJointsIds |> Set.member joint0Id then Just joint1Id
        else if outsetJointsIds |> Set.member joint1Id then Just joint0Id
        else Nothing)
      |> Set.fromList |> Set.union outsetJointsIds
  in
    if nextStepReachableJointsIds == outsetJointsIds
    then outsetJointsIds
    else getAllReachedJointsIdsFromOutset connectedJoints nextStepReachableJointsIds

locationIsInsideScenario : Float2 -> Bool
locationIsInsideScenario (x, y) = -1111 < y

getCurrentBuildingHeight : Model -> Float
getCurrentBuildingHeight scenario = scenario.joints |> Dict.values |> List.map (\joint -> joint.location |> Tuple.second) |> List.maximum |> Maybe.withDefault 0
