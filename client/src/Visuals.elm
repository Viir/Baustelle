module Visuals exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA exposing (transform, x1, x2, y1, y2)
import Html
import Html.Attributes exposing (style)
import Vector2 exposing (Float2)

type alias HtmlStyle = List (String, String)


svgGroupWithTranslationAndElements : Float2 -> List (Svg a) -> Svg a
svgGroupWithTranslationAndElements translation elements =
  svgGroupWithListTransformStringAndElements [svgTransformTranslate translation] elements

svgGroupWithListTransformStringAndElements : List String -> List (Svg a) -> Svg a
svgGroupWithListTransformStringAndElements listTransform elements =
  Svg.g [ transform (listTransform |> String.join " ") ] elements

svgTransformTranslate : Float2 -> String
svgTransformTranslate (offsetX, offsetY) = "translate(" ++ (toString offsetX) ++ "," ++ (toString offsetY) ++ ")"

svgListAttributesFromStartAndEnd : Float2 -> Float2 -> List (Svg.Attribute a)
svgListAttributesFromStartAndEnd (origX, origY) (destX, destY) =
  [ x1 (toString origX), y1 (toString origY), x2 (toString destX), y2 (toString destY) ]

svgPathDataFromPolygonListPoint : List Float2 -> String
svgPathDataFromPolygonListPoint polygonListPoint =
  let
    vector2String (a, b) = (toString a) ++ " " ++ (toString b)
  in
    case (polygonListPoint |> List.head, polygonListPoint |> List.tail) of
    (Just head, Just tail) -> "M" ++ (vector2String head) ++ " " ++ ((tail |> List.map (\point -> "L " ++ (vector2String point)) |> String.join " "))
    _ -> ""

svgCenteredText : String -> Float2 -> Float -> String -> Html.Html a
svgCenteredText text (x, y) fontSize color =
  Svg.text_ [ SA.x (x |> toString), SA.y (y |> toString), style (svgCenteredTextStyle fontSize color) ] [ Svg.text text ]

svgCenteredTextStyle : Float -> String -> HtmlStyle
svgCenteredTextStyle fontSize color =
  [
    ("text-anchor","middle"),("font-size", (fontSize |> toString) ++ "px"),("font-family","'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
    ("fill",color),("opacity","0.7")
  ]

svgRectAttributesSizeAll : List (Html.Attribute a)
svgRectAttributesSizeAll =
  [ SA.width "9999", SA.height "9999" ]
