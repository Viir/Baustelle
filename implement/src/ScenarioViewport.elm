module ScenarioViewport exposing (Event(..), State, ViewModel, defaultViewport, update, view, viewportSize)

import Color
import Color.Convert
import Console
import Dict
import GameConfig
import Html
import Scenario exposing (JointId)
import Set
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)
import Visuals exposing (HtmlStyle, svgGroupWithTranslationAndElements)


type Event
    = MouseEvent Console.MouseEvent
    | ErrorEvent String


type alias State =
    { mouseLocationInWorld : Maybe Float2
    , dragStartJointId : Maybe JointId
    }


type alias JointViewModel =
    { diameter : Float
    , isBuilt : Bool
    , indicateInteractivity : Bool
    , isReached : Bool
    , supportType : JointSupportType
    , strokeWidth : Float
    }


type alias ViewModel =
    { scenario : Scenario.State
    , scenarioAfterMouseUpEvent : Scenario.State
    , viewportAfterMouseDownEvent : State
    }


type JointSupportType
    = None
    | Temp
    | Perm


defaultViewport : State
defaultViewport =
    { mouseLocationInWorld = Nothing
    , dragStartJointId = Nothing
    }


jointViewDiameterDefault : Float
jointViewDiameterDefault =
    6


viewportSize : Float2
viewportSize =
    ( 800, 600 )


cameraTranslation : Float2
cameraTranslation =
    ( (viewportSize |> Tuple.first) / 2, (viewportSize |> Tuple.second) - 50 )


view : ViewModel -> Html.Html Event
view viewModel =
    let
        scenario =
            viewModel.scenario

        scenarioAfterMouseUpEvent =
            viewModel.scenarioAfterMouseUpEvent

        viewportAfterMouseDownEvent =
            viewModel.viewportAfterMouseDownEvent

        reachedJointsIds =
            Scenario.getAllReachedJointsIds scenario

        jointLocationFromId jointId =
            scenarioAfterMouseUpEvent.joints |> Dict.get jointId

        beamStartAndEndLocation : ( JointId, JointId ) -> Maybe ( Float2, Float2 )
        beamStartAndEndLocation ( startJointId, endJointId ) =
            case ( jointLocationFromId startJointId, jointLocationFromId endJointId ) of
                ( Just startJoint, Just endJoint ) ->
                    Just ( startJoint.location, endJoint.location )

                _ ->
                    Nothing

        jointsViews =
            scenario.joints
                |> Dict.union scenarioAfterMouseUpEvent.joints
                |> Dict.map
                    (\jointId joint ->
                        let
                            isBuilt =
                                scenario.joints |> Dict.member jointId

                            indicateInteractivity =
                                viewportAfterMouseDownEvent.dragStartJointId == Just jointId

                            supportType =
                                getSupportTypeFromJointId scenarioAfterMouseUpEvent jointId
                        in
                        jointView
                            { diameter = jointViewDiameterDefault
                            , isBuilt = isBuilt
                            , indicateInteractivity = indicateInteractivity && isBuilt
                            , supportType = supportType
                            , isReached = reachedJointsIds |> Set.member jointId
                            , strokeWidth = jointViewDiameterDefault / 16
                            }
                            joint.location
                    )
                |> Dict.values

        beamsViews =
            scenarioAfterMouseUpEvent.beams
                |> Dict.map
                    (\jointsIds beam ->
                        case beamStartAndEndLocation jointsIds of
                            Nothing ->
                                Nothing

                            Just location ->
                                let
                                    isBuilt =
                                        scenario.beams |> Dict.member jointsIds

                                    stressFactor =
                                        if not isBuilt then
                                            0

                                        else
                                            Scenario.stressFactorFromBeam scenario jointsIds |> Maybe.withDefault 1
                                in
                                Just (beamView isBuilt stressFactor location)
                    )
                |> Dict.values
                |> List.filterMap identity

        mouseEventOffsetTransform eventOffset =
            let
                ( translatedX, translatedY ) =
                    Vector2.sub eventOffset cameraTranslation
            in
            ( translatedX, -translatedY )

        inputElement : Html.Html Event
        inputElement =
            Svg.rect
                ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ]
                    |> List.append (Console.setMouseEventAttributeWithOffsetMapped mouseEventOffsetTransform)
                )
                []
                |> Html.map
                    (\maybeEvent ->
                        maybeEvent
                            |> Maybe.andThen (\event -> Just (MouseEvent event))
                            |> Maybe.withDefault (ErrorEvent "mouse event")
                    )

        adversariesViews =
            scenario.adversaries
                |> Dict.map
                    (\jointsIds adversary ->
                        let
                            location =
                                beamStartAndEndLocation jointsIds
                                    |> Maybe.andThen (\( joint0loc, joint1loc ) -> Vector2.add joint0loc joint1loc |> Vector2.scale 0.5 |> Just)
                                    |> Maybe.withDefault ( 0, 0 )
                        in
                        adversaryView adversary |> List.singleton |> Visuals.svgGroupWithTranslationAndElements location
                    )
                |> Dict.values
    in
    [ [ jointsViews
      , beamsViews
      , adversariesViews
      ]
        |> List.map (Svg.g [])
        |> Visuals.svgGroupWithListTransformStringAndElements [ "scale(1,-1)" ]
        |> List.singleton
        |> Visuals.svgGroupWithTranslationAndElements cameraTranslation
    , inputElement
    ]
        |> Svg.g []


update : Event -> Scenario.State -> State -> ( State, List Scenario.FromPlayerEvent )
update event scenarioBeforeUpdate viewport =
    updateWithScenarioUpdated event (scenarioBeforeUpdate |> Scenario.progress 0) viewport


updateWithScenarioUpdated : Event -> Scenario.State -> State -> ( State, List Scenario.FromPlayerEvent )
updateWithScenarioUpdated event scenario viewport =
    case event of
        MouseEvent mouseEvent ->
            let
                eventTypeSpecificTransform : State -> ( State, List Scenario.FromPlayerEvent )
                eventTypeSpecificTransform =
                    case mouseEvent.eventType of
                        Console.MouseDown ->
                            let
                                dragStartJointId =
                                    case getIdOfJointUnderMouse scenario viewport of
                                        Nothing ->
                                            Nothing

                                        Just jointUnderMouseId ->
                                            if Scenario.getAllReachedJointsIds scenario |> Set.member jointUnderMouseId then
                                                Just jointUnderMouseId

                                            else
                                                Nothing
                            in
                            \viewport1 -> ( { viewport1 | dragStartJointId = dragStartJointId }, [] )

                        Console.MouseUp ->
                            let
                                toScenarioMessage =
                                    case viewport.mouseLocationInWorld of
                                        Nothing ->
                                            []

                                        Just mouseLocationInWorld ->
                                            case ( viewport.dragStartJointId, getIdOfJointForInteractionFromLocation scenario mouseLocationInWorld ) of
                                                ( Just startJointId, Just endJointId ) ->
                                                    [ Scenario.BuildBeam startJointId endJointId ]

                                                ( Just startJointId, Nothing ) ->
                                                    let
                                                        newJointId =
                                                            (scenario.joints |> Dict.keys |> List.maximum |> Maybe.withDefault 0) + 1
                                                    in
                                                    [ Scenario.TempSupportForJoint newJointId mouseLocationInWorld, Scenario.BuildBeam startJointId newJointId ]

                                                _ ->
                                                    []
                            in
                            \viewport1 -> ( { viewport1 | dragStartJointId = Nothing }, toScenarioMessage )

                        _ ->
                            \viewport1 -> ( viewport1, [] )

                viewportAfterMouseMove =
                    { viewport | mouseLocationInWorld = Just mouseEvent.offset }
            in
            eventTypeSpecificTransform { viewportAfterMouseMove | mouseLocationInWorld = Just mouseEvent.offset }

        ErrorEvent _ ->
            ( viewport, [] )


getIdOfJointUnderMouse : Scenario.State -> State -> Maybe JointId
getIdOfJointUnderMouse scenario viewport =
    case viewport.mouseLocationInWorld of
        Nothing ->
            Nothing

        Just mouseLocationInWorld ->
            getIdOfJointForInteractionFromLocation scenario mouseLocationInWorld


getIdOfJointForInteractionFromLocation : Scenario.State -> Float2 -> Maybe JointId
getIdOfJointForInteractionFromLocation scenario location =
    scenario.joints
        |> Dict.filter (\_ joint -> Vector2.distance joint.location location < jointViewDiameterDefault * 2)
        |> Dict.keys
        |> List.head


jointView : JointViewModel -> Float2 -> Html.Html a
jointView viewmodel location =
    let
        diameter =
            viewmodel.diameter
                * (1
                    + (if viewmodel.indicateInteractivity then
                        0.3

                       else
                        0
                      )
                  )

        supportIndicationCircleScale =
            case viewmodel.supportType of
                None ->
                    Nothing

                Temp ->
                    Just 1.6

                Perm ->
                    Just 1.9

        supportIndicationVisuals =
            case supportIndicationCircleScale of
                Nothing ->
                    []

                Just scale ->
                    [ jointViewCircle { viewmodel | diameter = diameter * scale, strokeWidth = viewmodel.strokeWidth * 0.3 } ]
    in
    [ [ jointViewCircle { viewmodel | diameter = diameter } ]
    , supportIndicationVisuals
    ]
        |> List.concat
        |> svgGroupWithTranslationAndElements location


jointViewCircle : JointViewModel -> Html.Html a
jointViewCircle viewModel =
    Svg.circle
        (SA.r ((viewModel.diameter |> String.fromFloat) ++ "px") :: Visuals.styleAttributes (jointStyle viewModel))
        []


beamView : Bool -> Float -> ( Float2, Float2 ) -> Html.Html a
beamView isBuilt stressFactor ( startLocation, endLocation ) =
    Svg.line
        (Visuals.svgListAttributesFromStartAndEnd startLocation endLocation
            ++ Visuals.styleAttributes (beamLineStyle isBuilt stressFactor)
        )
        []


getSupportTypeFromJointId : Scenario.State -> JointId -> JointSupportType
getSupportTypeFromJointId scenario jointId =
    [ ( scenario.permSupport, Perm ), ( scenario.tempSupport, Temp ) ]
        |> List.filterMap
            (\( supportedSetDict, supportType ) ->
                if supportedSetDict |> Dict.keys |> List.member jointId then
                    Just supportType

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault None


adversaryView : Scenario.Adversary -> Html.Html a
adversaryView adversary =
    let
        adversaryShapeScaled =
            adversaryShape |> List.map (Vector2.scale ((adversary.mass / GameConfig.adversaryMass) ^ 0.5))
    in
    Svg.path (SA.d (Visuals.svgPathDataFromPolygonListPoint adversaryShapeScaled ++ "z") :: Visuals.styleAttributes adversaryStyle) []


adversaryShape : List Float2
adversaryShape =
    [ ( -2, 5 ), ( 2, 5 ), ( 2, -3 ), ( 6, -3 ), ( 0, -9 ), ( -6, -3 ), ( -2, -3 ) ]


jointStyle : JointViewModel -> HtmlStyle
jointStyle viewModel =
    [ ( "stroke", "whitesmoke" )
    , ( "stroke-opacity"
      , (if viewModel.isReached then
            0.6

         else
            0.2
        )
            |> String.fromFloat
      )
    , ( "stroke-width", (viewModel.strokeWidth * viewModel.diameter |> String.fromFloat) ++ "px" )
    , ( "stroke-dasharray"
      , if viewModel.isBuilt then
            "inherit"

        else
            viewModel.strokeWidth * viewModel.diameter |> String.fromFloat
      )
    , ( "fill", "none" )
    ]


beamLineStyle : Bool -> Float -> HtmlStyle
beamLineStyle isBuilt stressFactor =
    let
        color =
            Color.hsla 0 (stressFactor |> min 1 |> max 0) 0.5 0.8
    in
    [ ( "stroke", color |> Color.Convert.colorToCssRgba )
    , ( "stroke-width", (jointViewDiameterDefault / 2 |> String.fromFloat) ++ "px" )
    , ( "stroke-opacity", "0.6" )
    , ( "stroke-dasharray"
      , if isBuilt then
            "inherit"

        else
            jointViewDiameterDefault / 2 |> String.fromFloat
      )
    ]


adversaryStyle : HtmlStyle
adversaryStyle =
    [ ( "fill", "orange" ), ( "opacity", "0.7" ) ]
