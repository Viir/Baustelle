module Game exposing (Event, State, initialGame, progress, update, view)

import Base exposing (..)
import Color
import Color.Convert
import Console
import Dict
import GameConfig
import Html
import Html.Attributes as HA
import Random
import Scenario
import ScenarioConstruction
import ScenarioViewport
import Set
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)
import Visuals exposing (HtmlStyle)


type alias State =
    { scenario : Scenario.State
    , toDefendJointId : Scenario.JointId
    , viewport : ScenarioViewport.State
    , suppliesGiven : Float
    }


type Event
    = ScenarioViewport ScenarioViewport.Event


versionTextAndUrl : ( String, String )
versionTextAndUrl =
    ( "Baustelle v2021-03-06 - Gamelab", "https://gamelab.zone/" )


initialScenario : Scenario.State
initialScenario =
    let
        jointDistanceHorizontal =
            150

        rowFromJointCountAndHeight ( jointCount, height ) =
            List.range 0 (jointCount - 1) |> List.map (\i -> ( i * jointDistanceHorizontal - ((jointCount - 1) * jointDistanceHorizontal // 2) |> toFloat, height ))

        supportJointsLocations =
            rowFromJointCountAndHeight ( 4, 0 )

        towerJointsLocations =
            [ ( 3, 130 ), ( 2, 260 ), ( 1, 450 ) ] |> List.map rowFromJointCountAndHeight |> List.concat

        emptyScenario =
            ScenarioConstruction.emptyScenario
    in
    { emptyScenario | supplies = 2000 }
        |> ScenarioConstruction.withPermSupportAddedAtLocations
            (supportJointsLocations |> List.map (\location -> ( location, True )))
        |> ScenarioConstruction.withJointsAddedAtLocations towerJointsLocations
        |> ScenarioConstruction.withBeamsAddedForMaxLength 240
        |> Scenario.progress 1000


initialGame : State
initialGame =
    initialScenario |> gameFromScenario


gameFromScenario : Scenario.State -> State
gameFromScenario scenario =
    let
        -- pick the highest joint as the joint to defend.
        toDefendJointId =
            scenario.joints
                |> Dict.toList
                |> List.sortBy (\( _, joint ) -> joint.location |> Tuple.second)
                |> List.reverse
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault -9999
    in
    { scenario = scenario
    , toDefendJointId = toDefendJointId
    , viewport = ScenarioViewport.defaultViewport
    , suppliesGiven = 0
    }


adversaryAdditionAverageDistance : Int
adversaryAdditionAverageDistance =
    4000


progress : Int -> Random.Seed -> State -> State
progress duration randomSeed game =
    if game |> isGameOver then
        game

    else
        let
            addAdversary =
                (Random.step (Random.int 0 adversaryAdditionAverageDistance) randomSeed |> Tuple.first) < duration

            suppliesToAdd =
                (game.scenario.timeMilli |> toFloat) ^ 0.8 * 0.1 - game.suppliesGiven

            scenario =
                if addAdversary then
                    game.scenario |> withAdversaryAddedAtRandomLocation randomSeed

                else
                    game.scenario

            scenarioWithSuppliesAdded =
                { scenario | supplies = scenario.supplies + suppliesToAdd }
        in
        { game
            | scenario = scenarioWithSuppliesAdded |> Scenario.progress duration
            , suppliesGiven = game.suppliesGiven + suppliesToAdd
        }


withAdversaryAddedAtRandomLocation : Random.Seed -> Scenario.State -> Scenario.State
withAdversaryAddedAtRandomLocation seed scenario =
    let
        jointsToAvoidIds =
            scenario.permSupport |> Dict.keys |> Set.fromList

        locationsToChooseFromWithWeight =
            scenario.beams
                |> Dict.toList
                |> List.filterMap
                    (\( ( joint0, joint1 ), beam ) ->
                        if [ joint0, joint1 ] |> List.all (\joint -> jointsToAvoidIds |> Set.member joint) then
                            Nothing

                        else
                            Just ( ( joint0, joint1 ), beam.builtLength |> round )
                    )
    in
    case locationsToChooseFromWithWeight |> pickRandomItemFromListWeighted seed of
        Nothing ->
            scenario

        Just adversaryLocation ->
            scenario |> Scenario.withAdversaryAddedOnBeam ( adversaryLocation, GameConfig.adversaryMass )


update : Event -> State -> State
update event game =
    if game |> isGameOver then
        game

    else
        case event of
            ScenarioViewport viewportEvent ->
                let
                    ( viewport, listToGameInput ) =
                        ScenarioViewport.update viewportEvent game.scenario game.viewport

                    scenario =
                        game.scenario |> Scenario.updateForPlayerInputs listToGameInput
                in
                { game | scenario = scenario, viewport = viewport }


view : State -> Html.Html Event
view gameBeforeUpdate =
    let
        game =
            { gameBeforeUpdate | scenario = gameBeforeUpdate.scenario |> Scenario.progress 0 }

        resultFromMouseEventAtCurrentLocation =
            getResultFromMouseEventAtCurrentLocation game

        scenarioViewModel =
            { scenario = game.scenario
            , scenarioAfterMouseUpEvent = (resultFromMouseEventAtCurrentLocation Console.MouseUp).scenario
            , viewportAfterMouseDownEvent = (resultFromMouseEventAtCurrentLocation Console.MouseDown).viewport
            }

        ( viewportWidth, viewportHeight ) =
            ScenarioViewport.viewportSize

        suppliesChangeOnMouseUpRounded : Int
        suppliesChangeOnMouseUpRounded =
            scenarioViewModel.scenarioAfterMouseUpEvent.supplies - scenarioViewModel.scenario.supplies |> ceiling

        ( suppliesChangeColorSign, suppliesChangeColorHue ) =
            if suppliesChangeOnMouseUpRounded < 0 then
                ( "-", 0 )

            else
                ( "+", 2 )

        suppliesChangeColor =
            Color.hsla suppliesChangeColorHue 0.7 0.6 0.8
                |> Color.Convert.colorToCssRgba

        suppliesChangeListTextWithColor =
            if suppliesChangeOnMouseUpRounded == 0 then
                []

            else
                [ ( suppliesChangeColorSign ++ " " ++ (suppliesChangeOnMouseUpRounded |> abs |> String.fromInt), suppliesChangeColor ) ]

        suppliesView =
            [ [ ( scenarioViewModel.scenario.supplies |> round |> String.fromInt, "whitesmoke" ) ]
            , suppliesChangeListTextWithColor
            ]
                |> List.concat
                |> List.indexedMap
                    (\i ( text, color ) ->
                        Visuals.svgCenteredText (text ++ " $") ( viewportWidth / 2 + (i |> toFloat) * 80, 30 ) 20 color
                    )
                |> Svg.g []

        ( instructionViewElements, gameOverViewElements ) =
            if game |> isGameOver then
                ( []
                , [ Svg.rect
                        (Visuals.svgRectAttributesSizeAll
                            ++ Visuals.styleAttributes [ ( "fill", "black" ), ( "opacity", "0.5" ) ]
                        )
                        []
                  , gameOverView game
                  ]
                )

            else
                ( [ instructionsView ], [] )
    in
    [ ScenarioViewport.view scenarioViewModel |> Html.map ScenarioViewport
    , [ suppliesView ] |> List.append instructionViewElements |> Svg.g [ HA.style "pointer-events" "none" ]
    , gameOverViewElements |> Svg.g []
    , versionView
    ]
        |> Svg.svg
            (SA.width (viewportWidth |> String.fromFloat)
                :: SA.height (viewportHeight |> String.fromFloat)
                :: Visuals.styleAttributes viewportStyle
            )


instructionsView : Html.Html a
instructionsView =
    let
        fontSize =
            16
    in
    instructionTextLines
        |> List.indexedMap (\i textLine -> Visuals.svgCenteredText textLine ( 0, fontSize * 1.3 * (i |> toFloat) ) fontSize "whitesmoke")
        |> Visuals.svgGroupWithTranslationAndElements ( (ScenarioViewport.viewportSize |> Tuple.first) / 2, 70 )


versionView : Html.Html a
versionView =
    let
        ( viewportWidth, viewportHeight ) =
            ScenarioViewport.viewportSize

        styleList =
            [ ( "text-anchor", "end" )
            , ( "font-size", "13px" )
            , ( "font-family", Visuals.cssFontFamily )
            , ( "fill", "whitesmoke" )
            , ( "opacity", "0.3" )
            , ( "cursor", "pointer" )
            ]
    in
    [ Svg.text (versionTextAndUrl |> Tuple.first) ]
        |> Svg.text_
            (SA.x (viewportWidth - 30 |> String.fromFloat)
                :: SA.y (viewportHeight - 16 |> String.fromFloat)
                :: Visuals.styleAttributes styleList
            )
        |> List.singleton
        |> Svg.a [ SA.xlinkHref (versionTextAndUrl |> Tuple.second) ]


gameOverView : State -> Html.Html a
gameOverView game =
    [ ( "GAME OVER", ( 60, 0 ) )
    , ( "you kept up for " ++ ((game.scenario.timeMilli // 1000) |> String.fromInt) ++ " seconds", ( 18, 34 ) )
    ]
        |> List.map (\( text, ( fontSize, offsetVertical ) ) -> Visuals.svgCenteredText text ( 0, offsetVertical ) fontSize "whitesmoke")
        |> Visuals.svgGroupWithTranslationAndElements ( (ScenarioViewport.viewportSize |> Tuple.first) / 2, 100 )


updateForPlayerInputs : List Scenario.FromPlayerEvent -> State -> State
updateForPlayerInputs listFromPlayerInput game =
    { game | scenario = game.scenario |> Scenario.updateForPlayerInputs listFromPlayerInput }


isGameOver : State -> Bool
isGameOver game =
    game.scenario.joints
        |> Dict.get game.toDefendJointId
        |> Maybe.andThen (\toDefendJoint -> (toDefendJoint.location |> Tuple.second) < 0 |> Just)
        |> Maybe.withDefault True


getResultFromMouseEventAtCurrentLocation : State -> Console.MouseEventType -> State
getResultFromMouseEventAtCurrentLocation game eventType =
    case game.viewport.mouseLocationInWorld of
        Nothing ->
            game

        Just mouseLocationInWorld ->
            game |> update (getMouseLeftButtonEventForOffset eventType mouseLocationInWorld |> ScenarioViewport.MouseEvent |> ScenarioViewport)


getMouseLeftButtonEventForOffset : Console.MouseEventType -> Float2 -> Console.MouseEvent
getMouseLeftButtonEventForOffset eventType offset =
    { button = Console.Left
    , eventType = eventType
    , offset = offset
    , wheelDelta = ( 0, 0 )
    }


instructionTextLines : List String
instructionTextLines =
    [ "Prevent the tower from collapsing.", "Use the mouse cursor and drag to add beams to reinforce the structure." ]


viewportStyle : HtmlStyle
viewportStyle =
    [ ( "background", "#101010" )
    , ( "cursor", "default" )
    , ( "user-select", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ie-user-select", "none" )
    ]
