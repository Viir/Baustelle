module ScenarioConstruction exposing (..)
-- Functions for constructing a scenario/gameworld.

import Base exposing (..)
import Scenario exposing (..)
import Dict
import Set
import Vector2 exposing (Float2)


emptyScenario : Scenario.Model
emptyScenario =
    {
        joints = Dict.empty,
        permSupport = Dict.empty,
        tempSupport = Dict.empty,
        components = Dict.empty,
        outsetJoints = Set.empty,
        tempSupportRange = 0
    }

withPermSupportAddedAtLocations : List (Float2, Bool) -> Scenario.Model -> Scenario.Model
withPermSupportAddedAtLocations supportLocations scenario =
    let
      jointIdBase = ([ scenario.joints |> Dict.keys, scenario.permSupport |> Dict.keys ] |> List.concat |> List.maximum |> Maybe.withDefault 0) + 1

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

withTempSupportRange : Float -> Scenario.Model -> Scenario.Model
withTempSupportRange range scenario = { scenario | tempSupportRange = range }
