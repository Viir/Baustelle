module ScenarioConstruction exposing (..)
-- Functions for constructing a scenario/gameworld.

import Scenario exposing (..)
import Dict
import Vector2 exposing (Float2)


emptyScenario : Scenario.Model
emptyScenario =
    {
        joints = Dict.empty,
        permSupport = Dict.empty,
        tempSupport = Dict.empty,
        components = Dict.empty
    }

withPermSupportAddedAtLocations : List Float2 -> Scenario.Model -> Scenario.Model
withPermSupportAddedAtLocations supportLocations scenario =
    let
      jointIdBase = ([ scenario.joints |> Dict.keys, scenario.permSupport |> Dict.keys ] |> List.concat |> List.maximum |> Maybe.withDefault 0) + 1

      permSupportAddition =
        supportLocations
        |> List.indexedMap (\i supportLocation -> (jointIdBase + i, supportLocation))
        |> Dict.fromList
    in
      { scenario | permSupport = scenario.permSupport |> Dict.union permSupportAddition }
