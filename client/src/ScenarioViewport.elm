module ScenarioViewport exposing (Model, Msg, update, view, defaultViewport)

import Scenario exposing (JointId)
import Console
import Visuals exposing (HtmlStyle, svgGroupWithTranslationAndElements)
import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)
import Dict
import Set
import Color
import Color.Convert


type Msg
  = MouseEvent Console.MouseEvent
  | Error String

type alias Model =
  {
    mouseLocationInWorld : Maybe Float2,
    dragStartJointId : Maybe JointId
  }

type alias JointViewModel =
  {
    diameter : Float,
    isBuilt : Bool,
    indicateInteractivity : Bool,
    isReached : Bool,
    supportType : JointSupportType,
    strokeWidth : Float
  }

type JointSupportType = None | Temp | Perm

defaultViewport : Model
defaultViewport =
  {
    mouseLocationInWorld = Nothing,
    dragStartJointId = Nothing
  }

jointViewDiameterDefault : Float
jointViewDiameterDefault = 6

view : Scenario.Model -> Model -> Html.Html Msg
view scenarioBeforeUpdate viewport =
  viewWithScenarioUpdated (scenarioBeforeUpdate |> Scenario.progress 0) viewport

viewportSize : Float2
viewportSize = (800, 600)

cameraTranslation : Float2
cameraTranslation = ((viewportSize |> Tuple.first) / 2, (viewportSize |> Tuple.second) - 100)

viewWithScenarioUpdated : Scenario.Model -> Model -> Html.Html Msg
viewWithScenarioUpdated scenario viewport =
  let
    resultFromMouseEventAtCurrentLocation = getResultFromMouseEventAtCurrentLocation scenario viewport
    (scenarioAfterMouseUpEvent, _) = resultFromMouseEventAtCurrentLocation Console.MouseUp
    (_, viewportAfterMouseDownEvent) = resultFromMouseEventAtCurrentLocation Console.MouseDown

    reachedJointsIds = Scenario.getAllReachedJointsIds scenario

    jointLocationFromId jointId = scenarioAfterMouseUpEvent.joints |> Dict.get jointId

    componentStartAndEndLocation : (JointId, JointId) -> Maybe (Float2, Float2)
    componentStartAndEndLocation (startJointId, endJointId) =
      case (jointLocationFromId startJointId, jointLocationFromId endJointId) of
        (Just startJoint, Just endJoint) -> Just (startJoint.location, endJoint.location)
        _ -> Nothing

    jointsViews =
      scenario.joints |> Dict.union scenarioAfterMouseUpEvent.joints
      |> Dict.map (\jointId joint ->
        let
          isBuilt = scenario.joints |> Dict.member jointId
          indicateInteractivity = viewportAfterMouseDownEvent.dragStartJointId == Just jointId
          supportType = getSupportTypeFromJointId scenarioAfterMouseUpEvent jointId
        in
          jointView
            {
              diameter = jointViewDiameterDefault,
              isBuilt = isBuilt,
              indicateInteractivity = (indicateInteractivity && isBuilt),
              supportType = supportType,
              isReached = reachedJointsIds |> Set.member jointId,
              strokeWidth = jointViewDiameterDefault / 16
            } joint.location)
      |> Dict.values

    componentsViews =
      scenarioAfterMouseUpEvent.components
      |> Dict.map (\jointsIds component ->
        case componentStartAndEndLocation jointsIds of
        Nothing -> Nothing
        Just location ->
          let
            isBuilt = scenario.components |> Dict.member jointsIds
            stressFactor = if not isBuilt then 0 else Scenario.stressFactorFromComponent scenario jointsIds |> Maybe.withDefault 1
          in
            Just (componentView isBuilt stressFactor location))
      |> Dict.values |> List.filterMap identity

    mouseEventOffsetTransform eventOffset =
      let
        (translatedX, translatedY) = Vector2.sub eventOffset cameraTranslation
      in
        (translatedX, -translatedY)

    (viewportWidth, viewportHeight) = viewportSize

    inputElement : Html.Html Msg
    inputElement =
      Svg.rect ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ] |> List.append (Console.setMouseEventAttributeWithOffsetMapped mouseEventOffsetTransform)) []
      |> Html.map (\maybeEvent -> maybeEvent |> Maybe.andThen (\event -> Just (MouseEvent event)) |> Maybe.withDefault (Error "mouse event"))

    heightLines =
      [
        [ heightLineView (viewportWidth * 0.8) scenario.maxHeightRecord ],
        if scenarioAfterMouseUpEvent.maxHeightRecord /= scenario.maxHeightRecord
        then [ heightLineView (viewportWidth * 0.8) scenarioAfterMouseUpEvent.maxHeightRecord |> List.singleton |> Svg.g [ style [("opacity","0.5")]] ]
        else []
      ] |> List.concat
  in
    [
      [
        jointsViews |> Svg.g [],
        componentsViews |> Svg.g [],
        heightLines |> Svg.g []
      ]
      |> Visuals.svgGroupWithListTransformStringAndElements ["scale(1,-1)"] |> List.singleton
      |> Visuals.svgGroupWithTranslationAndElements cameraTranslation,
      inputElement
    ]
    |> Svg.svg [ SA.width (viewportWidth |> toString), SA.height (viewportHeight |> toString), style viewportStyle ]

update : Msg -> Scenario.Model -> Model -> (Model, List Scenario.FromPlayerMsg)
update msg scenarioBeforeUpdate viewport =
  updateWithScenarioUpdated msg (scenarioBeforeUpdate |> Scenario.progress 0) viewport

updateWithScenarioUpdated : Msg -> Scenario.Model -> Model -> (Model, List Scenario.FromPlayerMsg)
updateWithScenarioUpdated msg scenario viewport =
  case msg of
  MouseEvent mouseEvent ->
    let
      eventTypeSpecificTransform : Model -> (Model, List Scenario.FromPlayerMsg)
      eventTypeSpecificTransform =
        case mouseEvent.eventType of
        Console.MouseDown ->
          let
            dragStartJointId =
              case getIdOfJointUnderMouse scenario viewport of
              Nothing -> Nothing
              Just jointUnderMouseId ->
                if Scenario.getAllReachedJointsIds scenario |> Set.member jointUnderMouseId then Just jointUnderMouseId else Nothing
          in
            (\viewport -> ({ viewport | dragStartJointId = dragStartJointId }, []))
        Console.MouseUp ->
          let
            toScenarioMessage =
              case viewport.mouseLocationInWorld of
              Nothing -> []
              Just mouseLocationInWorld ->
                case (viewport.dragStartJointId, getIdOfJointForInteractionFromLocation scenario mouseLocationInWorld) of
                (Just startJointId, Just endJointId) ->
                  [ Scenario.BuildComponent startJointId endJointId ]
                (Just startJointId, Nothing) ->
                  let
                    newJointId = (scenario.joints |> Dict.keys |> List.maximum |> Maybe.withDefault 0) + 1
                  in
                    [ Scenario.TempSupportForJoint newJointId mouseLocationInWorld, Scenario.BuildComponent startJointId newJointId ]
                _ -> []
          in
            (\viewport -> ({ viewport | dragStartJointId = Nothing }, toScenarioMessage))
        _ -> (\viewport -> (viewport, []))

      viewportAfterMouseMove = { viewport | mouseLocationInWorld = Just mouseEvent.offset }
    in
      eventTypeSpecificTransform { viewportAfterMouseMove | mouseLocationInWorld = Just mouseEvent.offset }
  Error error -> (viewport, [])

getMouseLeftButtonEventForOffset : Console.MouseEventType -> Float2 -> Console.MouseEvent
getMouseLeftButtonEventForOffset eventType offset =
  {
    button = Console.Left,
    eventType = eventType,
    offset = offset,
    wheelDelta = (0,0)
  }

getResultFromMouseEventAtCurrentLocation : Scenario.Model -> Model -> Console.MouseEventType -> (Scenario.Model, Model)
getResultFromMouseEventAtCurrentLocation scenario viewport eventType =
  let
    (resultViewport, toScenarioInput) =
      case viewport.mouseLocationInWorld of
      Nothing -> (viewport, [])
      Just mouseLocationInWorld ->
        update (MouseEvent (getMouseLeftButtonEventForOffset eventType mouseLocationInWorld)) scenario viewport
  in
    (Scenario.updateForPlayerInputs toScenarioInput scenario, resultViewport)

getIdOfJointUnderMouse : Scenario.Model -> Model -> Maybe JointId
getIdOfJointUnderMouse scenario viewport =
  case viewport.mouseLocationInWorld of
  Nothing -> Nothing
  Just mouseLocationInWorld -> getIdOfJointForInteractionFromLocation scenario mouseLocationInWorld

getIdOfJointForInteractionFromLocation : Scenario.Model -> Float2 -> Maybe JointId
getIdOfJointForInteractionFromLocation scenario location =
  scenario.joints
  |> Dict.filter (\_ joint -> Vector2.distance joint.location location < jointViewDiameterDefault * 2)
  |> Dict.keys |> List.head

jointView : JointViewModel -> Float2 -> Html.Html a
jointView viewmodel location =
  let
    diameter = viewmodel.diameter * (1 + (if viewmodel.indicateInteractivity then 0.3 else 0))

    supportIndicationCircleScale =
      case viewmodel.supportType of
      None -> Nothing
      Temp -> Just 1.6
      Perm -> Just 1.9

    supportIndicationVisuals =
      case supportIndicationCircleScale of
      Nothing -> []
      Just scale -> [ jointViewCircle { viewmodel | diameter = diameter * scale, strokeWidth = viewmodel.strokeWidth * 0.3} ]
  in
    [
      [ jointViewCircle { viewmodel | diameter = diameter } ],
      supportIndicationVisuals
    ] |> List.concat |> svgGroupWithTranslationAndElements location

jointViewCircle : JointViewModel -> Html.Html a
jointViewCircle viewModel =
  Svg.circle [ SA.r ((viewModel.diameter |> toString) ++ "px"), style (jointStyle viewModel ) ] []

componentView : Bool -> Float -> (Float2, Float2) -> Html.Html a
componentView isBuilt stressFactor (startLocation, endLocation) =
  Svg.line ((Visuals.svgListAttributesFromStartAndEnd startLocation endLocation) |> List.append [style (componentLineStyle isBuilt stressFactor)]) []

getSupportTypeFromJointId : Scenario.Model -> JointId -> JointSupportType
getSupportTypeFromJointId scenario jointId =
  [ (scenario.permSupport, Perm), (scenario.tempSupport, Temp) ]
  |> List.filterMap (\(supportedSetDict, supportType) -> if supportedSetDict |> Dict.keys |> List.member jointId then Just supportType else Nothing)
  |> List.head |> Maybe.withDefault None

heightLineView : Float -> Float -> Html.Html a
heightLineView horizontalExtend height =
  [
    Svg.line (Visuals.svgListAttributesFromStartAndEnd (-horizontalExtend * 0.5, 0) (horizontalExtend * 0.47, 0) |> List.append [ style heightLineStyle] ) [],
    [ Svg.tspan [ SA.dy "0.5em" ] [ Svg.text (height |> floor |> toString) ] ]
    |> Svg.text_ [ style heightNumberStyle ]
    |> List.singleton |> svgGroupWithTranslationAndElements (horizontalExtend * 0.5, 0)
    |> List.singleton |> Visuals.svgGroupWithListTransformStringAndElements ["scale(1,-1)"]
  ] |> svgGroupWithTranslationAndElements (0, height)

jointStyle : JointViewModel -> HtmlStyle
jointStyle viewModel =
  [
    ("stroke","whitesmoke"),
    ("stroke-opacity", (if viewModel.isReached then 0.6 else 0.2) |> toString),
    ("stroke-width", (viewModel.strokeWidth * viewModel.diameter |> toString) ++ "px"),
    ("stroke-dasharray", if viewModel.isBuilt then "inherit" else (viewModel.strokeWidth * viewModel.diameter |> toString)),
    ("fill","none")
  ]

componentLineStyle : Bool -> Float -> HtmlStyle
componentLineStyle isBuilt stressFactor =
  let
    color = Color.hsla 0 (stressFactor |> min 1 |> max 0) 0.5 0.8
  in
    [
      ("stroke", color |> Color.Convert.colorToCssRgba),("stroke-width", (jointViewDiameterDefault / 2 |> toString) ++ "px"),("stroke-opacity","0.6"),
      ("stroke-dasharray", if isBuilt then "inherit" else (jointViewDiameterDefault / 2 |> toString))
    ]

viewportStyle : HtmlStyle
viewportStyle =
    [("background", "#101010"), ("cursor","default")]

heightLineStyle : HtmlStyle
heightLineStyle = [ ("stroke","whitesmoke"),("stroke-width","3px"),("stroke-opacity","0.18"),("stroke-dasharray","4")]

heightNumberStyle : HtmlStyle
heightNumberStyle =
  [
    ("text-anchor","middle"),("font-size","30px"),("font-family","'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
    ("fill","whitesmoke"),("opacity","0.7")
  ]
