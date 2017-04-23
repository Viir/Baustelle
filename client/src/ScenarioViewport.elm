module ScenarioViewport exposing (Model, Msg, update, view, defaultViewport)

import Scenario exposing (JointId, update)
import Console
import Visuals exposing (HtmlStyle, svgGroupWithTranslationAndElements)
import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)
import Dict


type Msg
  = MouseEvent Console.MouseEvent
  | Error String

type alias Model =
  {
    mouseLocationInWorld : Maybe Float2,
    dragStartJointId : Maybe JointId
  }

defaultViewport : Model
defaultViewport =
  {
    mouseLocationInWorld = Nothing,
    dragStartJointId = Nothing
  }

jointViewDiameter : Float
jointViewDiameter = 8

view : Scenario.Model -> Model -> Html.Html Msg
view scenarioBeforeUpdate viewport =
  viewWithScenarioUpdated (scenarioBeforeUpdate |> Scenario.update) viewport

viewWithScenarioUpdated : Scenario.Model -> Model -> Html.Html Msg
viewWithScenarioUpdated scenario viewport =
  let
    jointLocationFromId jointId = scenario.joints |> Dict.get jointId

    supportJointsViews =
      scenario.joints
      |> Dict.map (\jointId jointLocation ->
        let
          isMouseOver = getIdOfJointUnderMouse scenario viewport == Just jointId
        in
          (jointView isMouseOver) |> svgGroupWithTranslationAndElements jointLocation)
      |> Dict.values

    dragGestureIndication : List (Html.Html a)
    dragGestureIndication =
      case (viewport.dragStartJointId |> Maybe.andThen jointLocationFromId, viewport.mouseLocationInWorld) of
      (Just dragStartLocation, Just mouseLocationInWorld) ->
        [ componentView False (dragStartLocation, mouseLocationInWorld) ]
      _ -> []

    componentsViews =
      scenario.components
      |> List.filterMap (\(startJointId, endJointId) ->
        case (jointLocationFromId startJointId, jointLocationFromId endJointId) of
        (Just startLocation, Just endLocation) -> Just (componentView True (startLocation, endLocation))
        _ -> Nothing)

    inputElement : Html.Html Msg
    inputElement =
      Svg.rect ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ] |> List.append Console.setMouseEventAttribute) []
      |> Html.map (\maybeEvent -> maybeEvent |> Maybe.andThen (\event -> Just (MouseEvent event)) |> Maybe.withDefault (Error "mouse event"))
  in
    Svg.svg [ SA.width "400", SA.height "400", style viewportStyle ]
    [
      supportJointsViews |> Svg.g [],
      componentsViews |> Svg.g [],
      dragGestureIndication |> Svg.g [],
      inputElement
    ]

update : Msg -> Scenario.Model -> Model -> (Model, List Scenario.FromPlayerMsg)
update msg scenarioBeforeUpdate viewport =
  updateWithScenarioUpdated msg (scenarioBeforeUpdate |> Scenario.update) viewport

updateWithScenarioUpdated : Msg -> Scenario.Model -> Model -> (Model, List Scenario.FromPlayerMsg)
updateWithScenarioUpdated msg scenario viewport =
  case msg of
  MouseEvent mouseEvent ->
    let
      eventTypeSpecificTransform : Model -> (Model, List Scenario.FromPlayerMsg)
      eventTypeSpecificTransform =
        case mouseEvent.eventType of
        Console.MouseDown -> (\viewport -> ({ viewport | dragStartJointId = getIdOfJointUnderMouse scenario viewport }, []))
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

getIdOfJointUnderMouse : Scenario.Model -> Model -> Maybe JointId
getIdOfJointUnderMouse scenario viewport =
  case viewport.mouseLocationInWorld of
  Nothing -> Nothing
  Just mouseLocationInWorld -> getIdOfJointForInteractionFromLocation scenario mouseLocationInWorld

getIdOfJointForInteractionFromLocation : Scenario.Model -> Float2 -> Maybe JointId
getIdOfJointForInteractionFromLocation scenario location =
  scenario.joints
  |> Dict.filter (\_ jointLocation -> Vector2.distance jointLocation location < jointViewDiameter * 2)
  |> Dict.keys |> List.head

jointView : Bool -> List (Html.Html a)
jointView isMouseOver =
  let
    diameter = jointViewDiameter * (1 + (if isMouseOver then 0.3 else 0))
  in
    [ Svg.circle [ SA.r ((diameter |> toString) ++ "px"), style (jointStyle diameter) ] []]

componentView : Bool -> (Float2, Float2) -> Html.Html a
componentView isBuilt (startLocation, endLocation) =
  Svg.line ((Visuals.svgListAttributesFromStartAndEnd startLocation endLocation) |> List.append [style (componentLineStyle isBuilt)]) []

jointStyle : Float -> HtmlStyle
jointStyle diameter =
  [("stroke","whitesmoke"),("stroke-opacity","0.7"),("stroke-width", (diameter / 3 |> toString) ++ "px")]

componentLineStyle : Bool -> HtmlStyle
componentLineStyle isBuilt =
  [
    ("stroke","whitesmoke"),("stroke-width", (jointViewDiameter / 3 |> toString) ++ "px"),("stroke-opacity","0.6"),
    ("stroke-dasharray", if isBuilt then "inherit" else (jointViewDiameter / 2 |> toString))
  ]

viewportStyle : HtmlStyle
viewportStyle =
    [("background", "#101010"), ("cursor","default")]
