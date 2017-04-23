module ScenarioViewport exposing (Model, Msg, update, view, defaultViewport)

import Scenario exposing (JointId)
import Console
import Visuals exposing (..)
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
    mouseLocationInWorld : Maybe Float2
  }

defaultViewport : Model
defaultViewport =
  {
    mouseLocationInWorld = Nothing
  }

jointViewDiameter : Float
jointViewDiameter = 8

view : Scenario.Model -> Model -> Html.Html Msg
view scenario viewport =
  let
    supportJointsViews =
      scenario.supportJoints
      |> Dict.map (\jointId jointLocation ->
        let
          isMouseOver = getIdOfJointUnderMouse scenario viewport == Just jointId
        in
          (jointView isMouseOver) |> svgGroupWithTranslationAndElements jointLocation)
      |> Dict.values

    inputElement : Html.Html Msg
    inputElement =
      Svg.rect ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ] |> List.append Console.setMouseEventAttribute) []
      |> Html.map (\maybeEvent -> maybeEvent |> Maybe.andThen (\event -> Just (MouseEvent event)) |> Maybe.withDefault (Error "mouse event"))
  in
    Svg.svg [ SA.width "400", SA.height "400", style [("background", "#101010") ]]
    [
      supportJointsViews |> Svg.g [],
      inputElement
    ]

update : Msg -> Scenario.Model -> Model -> Model
update msg scenario viewport =
  case msg of
  MouseEvent mouseEvent -> { viewport | mouseLocationInWorld = Just mouseEvent.offset }
  Error error -> viewport

getIdOfJointUnderMouse : Scenario.Model -> Model -> Maybe JointId
getIdOfJointUnderMouse scenario viewport =
  case viewport.mouseLocationInWorld of
  Nothing -> Nothing
  Just mouseLocationInWorld ->
    scenario.supportJoints
    |> Dict.filter (\_ jointLocation -> Vector2.distance jointLocation mouseLocationInWorld < jointViewDiameter * 2)
    |> Dict.keys |> List.head

jointView : Bool -> List (Html.Html a)
jointView isMouseOver =
  [ Svg.circle [ style (jointStyle isMouseOver) ] []]

jointStyle : Bool -> HtmlStyle
jointStyle isMouseOver =
  let
    diameter = jointViewDiameter * (1 + (if isMouseOver then 0.3 else 0))
  in
    [("r", (diameter |> toString) ++ "px"),("stroke","whitesmoke"),("stroke-opacity","0.7"),("stroke-width", (diameter / 3 |> toString) ++ "px")]
