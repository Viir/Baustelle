module Scenario exposing (Model, JointId, FromPlayerMsg (..), update, updateForPlayerInputs)

import Base exposing (..)
import Vector2 exposing (Float2)
import Dict

type alias JointId = Int

type alias Model =
  {
    joints : Dict.Dict JointId Float2,
    components : List (JointId, JointId),
    permSupport : Dict.Dict JointId Float2,
    tempSupport : Dict.Dict JointId Float2
  }

type FromPlayerMsg
  = BuildComponent JointId JointId
  | TempSupportForJoint JointId Float2

update : Model -> Model
update scenario =
  let
    joints =
      scenario.permSupport |> Dict.union scenario.tempSupport |> Dict.union scenario.joints
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
    else { scenario | components = scenario.components |> List.append [(startJointId, endJointId)]}
  TempSupportForJoint jointId supportLocation ->
    if scenario.joints |> Dict.member jointId
    then scenario -- We do not support changing location of existing joints using this imput.
    else
      let
        tempSupport = Dict.singleton jointId supportLocation -- Only one temp support is allowed for a given point in time.
      in
        { scenario | tempSupport = tempSupport }
    |> update
