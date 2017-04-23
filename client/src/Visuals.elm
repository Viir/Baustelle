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

