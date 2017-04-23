module Scenario exposing (Model, JointId)

import Vector2 exposing (Float2)
import Dict

type alias JointId = Int

type alias Model =
  {
    supportJoints : Dict.Dict JointId Float2
  }

