module GameConfig exposing (..)

import Vector2 exposing (Float2)


type alias ScenarioConfig =
    { gravity : Float2
    , maintainBeamLengthForceFactor : Float
    , dampFactor : Float
    , beamFailThreshold : Float
    }


adversaryMass : Float
adversaryMass =
    600


scenarioConfig : ScenarioConfig
scenarioConfig =
    { gravity = ( 0, -1.0e-4 )
    , maintainBeamLengthForceFactor = 16
    , dampFactor = 1.0e-4
    , beamFailThreshold = 0.3
    }


updateStepDuration : Int
updateStepDuration =
    10
