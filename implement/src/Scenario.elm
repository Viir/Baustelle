module Scenario exposing
    ( Adversary
    , Beam
    , FromPlayerEvent(..)
    , JointId
    , State
    , distanceFromJointsInScenario
    , getAllReachedJointsIds
    , getExpansionVelocityFromPairOfJoints
    , getForceTowardsJointAndMassFromBeam
    , getJointsFromSupport
    , progress
    , stressFactorFromBeam
    , updateForPlayerInputs
    , withAdversaryAddedOnBeam
    )

import Base exposing (..)
import Dict
import GameConfig exposing (ScenarioConfig, updateStepDuration)
import Set
import Vector2 exposing (Float2)


type alias JointId =
    Int


type alias State =
    { config : ScenarioConfig
    , timeMilli : Int
    , joints : Dict.Dict JointId Joint
    , beams : Dict.Dict ( JointId, JointId ) Beam
    , permSupport : Dict.Dict JointId Float2
    , tempSupport : Dict.Dict JointId Float2
    , outsetJoints : Set.Set JointId
    , adversaries : Dict.Dict ( JointId, JointId ) Adversary
    , supplies : Float
    }


type alias Joint =
    { location : Float2
    , velocity : Float2
    }


type alias Beam =
    { builtLength : Float
    }


type alias Adversary =
    { mass : Float }


type FromPlayerEvent
    = BuildBeam JointId JointId
    | TempSupportForJoint JointId Float2


progress : Int -> State -> State
progress duration scenario =
    let
        fullStepCount =
            duration // updateStepDuration

        remainderStepDuration =
            duration - (fullStepCount * updateStepDuration)

        listStepDuration =
            remainderStepDuration :: (updateStepDuration |> List.repeat fullStepCount)
    in
    scenario |> withListTransformApplied (listStepDuration |> List.map updateStep)


updateStep : Int -> State -> State
updateStep duration scenario =
    let
        durationFloat =
            duration |> toFloat

        config =
            scenario.config

        originalJoints =
            scenario.joints |> Dict.union (getJointsFromSupport scenario)

        joints : Dict.Dict JointId Joint
        joints =
            originalJoints
                |> Dict.map
                    (\jointId joint ->
                        let
                            connectedBeamsForcesAndMasses : List ( Float2, Float )
                            connectedBeamsForcesAndMasses =
                                scenario.beams
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( beamLocation, beam ) ->
                                            let
                                                ( beamJoint0Id, beamJoint1Id ) =
                                                    beamLocation

                                                beamOtherJointId =
                                                    if beamJoint0Id == jointId then
                                                        beamJoint1Id

                                                    else
                                                        beamJoint0Id
                                            in
                                            if (beamJoint0Id == jointId || beamJoint1Id == jointId) |> not then
                                                Nothing

                                            else
                                                case scenario.joints |> Dict.get beamOtherJointId of
                                                    Nothing ->
                                                        Nothing

                                                    Just otherJoint ->
                                                        let
                                                            beamAdversary =
                                                                scenario.adversaries |> Dict.get beamLocation
                                                        in
                                                        Just (getForceTowardsJointAndMassFromBeam config durationFloat joint otherJoint beam beamAdversary)
                                        )

                            connectedBeamsForce =
                                connectedBeamsForcesAndMasses
                                    |> List.map Tuple.first
                                    |> List.foldl Vector2.add ( 0, 0 )

                            connectedBeamsMass =
                                connectedBeamsForcesAndMasses
                                    |> List.map Tuple.second
                                    |> List.sum

                            -- Avoid division by zero for case of zero connected beams.
                            combinedAcceleration =
                                connectedBeamsForce |> Vector2.scale (1 / (connectedBeamsMass + 1.0e-6))

                            dampFactor =
                                (1 + config.dampFactor) ^ durationFloat

                            velocity =
                                joint.velocity |> Vector2.add combinedAcceleration |> Vector2.scale (1 / dampFactor)

                            location =
                                joint.location |> Vector2.add (joint.velocity |> Vector2.scale durationFloat)
                        in
                        { joint | location = location, velocity = velocity }
                    )
    in
    { scenario | joints = joints, timeMilli = scenario.timeMilli + duration }
        |> removeJointsOutsideScenario
        |> updateForFailure


getForceTowardsJointAndMassFromBeam : ScenarioConfig -> Float -> Joint -> Joint -> Beam -> Maybe Adversary -> ( Float2, Float )
getForceTowardsJointAndMassFromBeam config durationFloat affectedJoint otherJoint beam maybeAdversary =
    let
        gravityAcceleration =
            config.gravity |> Vector2.scale durationFloat

        beamAdversaryMass =
            maybeAdversary |> Maybe.andThen (\adversary -> Just adversary.mass) |> Maybe.withDefault 0

        beamLength =
            affectedJoint.location |> Vector2.distance otherJoint.location

        beamCurrentMass =
            beamLength + beamAdversaryMass

        beamExpansionDirection =
            Vector2.directionFromTo otherJoint.location affectedJoint.location

        beamExpansionVelocity =
            getExpansionVelocityFromPairOfJoints affectedJoint otherJoint

        beamExpansionForceAmount =
            (beam.builtLength / beamLength - beamExpansionVelocity) - 1

        beamGravityForce =
            gravityAcceleration |> Vector2.scale beamCurrentMass

        beamMaintainLengthForce =
            beamExpansionDirection |> Vector2.scale (beamExpansionForceAmount * config.maintainBeamLengthForceFactor)
    in
    ( beamMaintainLengthForce |> Vector2.add beamGravityForce, beamCurrentMass )


getExpansionVelocityFromPairOfJoints : Joint -> Joint -> Float
getExpansionVelocityFromPairOfJoints joint0 joint1 =
    Vector2.dot (Vector2.sub joint1.velocity joint0.velocity) (Vector2.directionFromTo joint0.location joint1.location)


removeJointsOutsideScenario : State -> State
removeJointsOutsideScenario scenario =
    { scenario | joints = scenario.joints |> Dict.filter (\_ joint -> locationIsInsideScenario joint.location) }


updateForFailure : State -> State
updateForFailure scenario =
    let
        remainingBeams =
            scenario.beams
                |> Dict.filter
                    (\jointsIds beam ->
                        stressFactorFromBeam scenario jointsIds |> Maybe.andThen (\stressFactor -> Just (stressFactor < 1)) |> Maybe.withDefault False
                    )

        remainingBeamsKeys =
            remainingBeams |> Dict.keys

        supportJointsIds =
            [ scenario.permSupport |> Dict.keys, scenario.tempSupport |> Dict.keys ] |> List.concat |> Set.fromList

        remainingJoints =
            scenario.joints
                |> Dict.filter
                    (\jointId joint ->
                        (supportJointsIds |> Set.member jointId) || (remainingBeamsKeys |> List.any (\( joint0Id, joint1Id ) -> joint0Id == jointId || joint1Id == jointId))
                    )
    in
    { scenario | beams = remainingBeams, joints = remainingJoints }
        |> withAdversariesRemovedWhereBeamDoesNotExist


withAdversariesRemovedWhereBeamDoesNotExist : State -> State
withAdversariesRemovedWhereBeamDoesNotExist scenario =
    let
        remainingAdversaries =
            scenario.adversaries
                |> Dict.filter (\location _ -> scenario.beams |> Dict.member location)
    in
    { scenario | adversaries = remainingAdversaries }


withAdversaryAddedOnBeam : ( ( JointId, JointId ), Float ) -> State -> State
withAdversaryAddedOnBeam ( beamLocation, adversaryMass ) scenario =
    let
        previousAdversary =
            scenario.adversaries |> Dict.get beamLocation |> Maybe.withDefault emptyAdversary

        adversary =
            { previousAdversary | mass = previousAdversary.mass + adversaryMass }
    in
    { scenario | adversaries = scenario.adversaries |> Dict.insert beamLocation adversary }


getJointsFromSupport : State -> Dict.Dict JointId Joint
getJointsFromSupport scenario =
    scenario.permSupport
        |> Dict.union scenario.tempSupport
        |> Dict.map (\_ location -> { location = location, velocity = ( 0, 0 ) })


updateForPlayerInputs : List FromPlayerEvent -> State -> State
updateForPlayerInputs listFromPlayerInput scenario =
    scenario |> withListTransformApplied (listFromPlayerInput |> List.map updateForPlayerInput)


updateForPlayerInput : FromPlayerEvent -> State -> State
updateForPlayerInput event scenario =
    case event of
        BuildBeam startJointId endJointId ->
            let
                startJointIsReached =
                    getAllReachedJointsIds scenario |> Set.member startJointId
            in
            if startJointId == endJointId || not startJointIsReached then
                scenario

            else
                case distanceFromJointsInScenario scenario ( startJointId, endJointId ) of
                    Just beamLength ->
                        let
                            beam =
                                { builtLength = beamLength }

                            supplies =
                                scenario.supplies - beamLength

                            scenarioWithBeamRemoved =
                                scenario |> withBeamRemovedAtLocation ( startJointId, endJointId )
                        in
                        if supplies < 0 then
                            scenario

                        else
                            { scenarioWithBeamRemoved | supplies = supplies, beams = scenario.beams |> Dict.insert ( startJointId, endJointId ) beam }

                    _ ->
                        scenario

        TempSupportForJoint jointId supportLocation ->
            if scenario.joints |> Dict.member jointId then
                scenario
                -- We do not support changing location of existing joints using this imput.

            else
                let
                    tempSupport =
                        Dict.singleton jointId supportLocation

                    -- Only one temp support is allowed for a given point in time.
                in
                { scenario | tempSupport = tempSupport }
                    |> progress 0


withBeamRemovedAtLocation : ( JointId, JointId ) -> State -> State
withBeamRemovedAtLocation location scenario =
    let
        beams =
            scenario.beams |> Dict.remove location |> Dict.remove (location |> tuple2Swap)
    in
    { scenario | beams = beams }
        |> withAdversariesRemovedWhereBeamDoesNotExist


distanceFromJointsInScenario : State -> ( JointId, JointId ) -> Maybe Float
distanceFromJointsInScenario scenario ( joint0Id, joint1Id ) =
    case ( scenario.joints |> Dict.get joint0Id, scenario.joints |> Dict.get joint1Id ) of
        ( Just joint0, Just joint1 ) ->
            Just (joint0.location |> Vector2.distance joint1.location)

        _ ->
            Nothing


stressFactorFromBeam : State -> ( JointId, JointId ) -> Maybe Float
stressFactorFromBeam scenario joints =
    case ( distanceFromJointsInScenario scenario joints, scenario.beams |> Dict.get joints ) of
        ( Just length, Just beam ) ->
            let
                stretchFactor =
                    length / beam.builtLength - 1
            in
            Just (abs stretchFactor / scenario.config.beamFailThreshold)

        _ ->
            Nothing


getAllReachedJointsIds : State -> Set.Set JointId
getAllReachedJointsIds scenario =
    getAllReachedJointsIdsFromOutset (scenario.beams |> Dict.keys) scenario.outsetJoints


getAllReachedJointsIdsFromOutset : List ( JointId, JointId ) -> Set.Set JointId -> Set.Set JointId
getAllReachedJointsIdsFromOutset connectedJoints outsetJointsIds =
    let
        nextStepReachableJointsIds =
            connectedJoints
                |> List.filterMap
                    (\( joint0Id, joint1Id ) ->
                        if outsetJointsIds |> Set.member joint0Id then
                            Just joint1Id

                        else if outsetJointsIds |> Set.member joint1Id then
                            Just joint0Id

                        else
                            Nothing
                    )
                |> Set.fromList
                |> Set.union outsetJointsIds
    in
    if nextStepReachableJointsIds == outsetJointsIds then
        outsetJointsIds

    else
        getAllReachedJointsIdsFromOutset connectedJoints nextStepReachableJointsIds


locationIsInsideScenario : Float2 -> Bool
locationIsInsideScenario ( x, y ) =
    -1111 < y


getCurrentBuildingHeight : State -> Float
getCurrentBuildingHeight scenario =
    scenario.joints |> Dict.values |> List.map (\joint -> joint.location |> Tuple.second) |> List.maximum |> Maybe.withDefault 0


emptyAdversary : Adversary
emptyAdversary =
    { mass = 0 }
