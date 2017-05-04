module Game exposing (Model, updateForPlayerInputs, progress)

import Base exposing (..)
import Scenario
import Random
import Dict
import Set


type alias Model =
    {
        scenario : Scenario.Model
    }

adversaryAdditionAverageDistance : Int
adversaryAdditionAverageDistance = 4000

progress : Int -> Random.Seed -> Model -> Model
progress duration randomSeed game =
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

updateForPlayerInputs : List Scenario.FromPlayerMsg -> Model -> Model
updateForPlayerInputs listFromPlayerInput game =
    { game | scenario = (game.scenario |> Scenario.updateForPlayerInputs listFromPlayerInput)}

