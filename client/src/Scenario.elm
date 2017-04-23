module Scenario exposing (Model, JointId, FromPlayerMsg (..), updateForPlayerInput)

import Vector2 exposing (Float2)
import Dict

type alias JointId = Int

type alias Model =
  {
    supportJoints : Dict.Dict JointId Float2,
    components : List (JointId, JointId)
  }

type FromPlayerMsg
  = BuildComponent JointId JointId

updateForPlayerInput : FromPlayerMsg -> Model -> Model
updateForPlayerInput msg scenario =
  case msg of
  BuildComponent startJointId endJointId ->
    if startJointId == endJointId
    then scenario
    else { scenario | components = scenario.components |> List.append [(startJointId, endJointId)]}
