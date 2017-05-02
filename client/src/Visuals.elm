module Visuals exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
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
