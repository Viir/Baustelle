module ScenarioViewport exposing (Model, Msg, update, view, defaultViewport)

import Scenario
import Console
import Visuals exposing (..)
import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)


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

view : Scenario.Model -> Model -> Html.Html Msg
view scenario viewport =
  let
    supportPointsViews =
      scenario.supportPoints
      |> List.map (\location -> [ Svg.circle [ SA.r "4", style supportPointStyle ] []] |> svgGroupWithTranslationAndElements location)

    inputElement : Html.Html Msg
    inputElement =
      Svg.rect ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ] |> List.append Console.setMouseEventAttribute) []
      |> Html.map (\maybeEvent -> maybeEvent |> Maybe.andThen (\event -> Just (MouseEvent event)) |> Maybe.withDefault (Error "mouse event"))
  in
    Svg.svg [ SA.width "400", SA.height "400", style [("background", "#101010") ]]
    [
      supportPointsViews |> Svg.g [],
      inputElement
    ]

update : Msg -> Model -> Model
update msg viewport =
  case msg of
  MouseEvent mouseEvent ->
    { viewport | mouseLocationInWorld = Just mouseEvent.offset }
  Error error -> viewport


supportPointStyle : HtmlStyle
supportPointStyle = [("r", "8px"),("stroke","whitesmoke"),("stroke-opacity","0.7"),("stroke-width","3px")]
