module Visuals exposing (..)

import Html
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA exposing (transform, x1, x2, y1, y2)
import Vector2 exposing (Float2)


type alias HtmlStyle =
    List ( String, String )


svgGroupWithTranslationAndElements : Float2 -> List (Svg a) -> Svg a
svgGroupWithTranslationAndElements translation elements =
    svgGroupWithListTransformStringAndElements [ svgTransformTranslate translation ] elements


svgGroupWithListTransformStringAndElements : List String -> List (Svg a) -> Svg a
svgGroupWithListTransformStringAndElements listTransform elements =
    Svg.g [ transform (listTransform |> String.join " ") ] elements


svgTransformTranslate : Float2 -> String
svgTransformTranslate ( offsetX, offsetY ) =
    "translate(" ++ String.fromFloat offsetX ++ "," ++ String.fromFloat offsetY ++ ")"


svgListAttributesFromStartAndEnd : Float2 -> Float2 -> List (Svg.Attribute a)
svgListAttributesFromStartAndEnd ( origX, origY ) ( destX, destY ) =
    [ x1 (String.fromFloat origX), y1 (String.fromFloat origY), x2 (String.fromFloat destX), y2 (String.fromFloat destY) ]


svgPathDataFromPolygonListPoint : List Float2 -> String
svgPathDataFromPolygonListPoint polygonListPoint =
    let
        vector2String ( a, b ) =
            String.fromFloat a ++ " " ++ String.fromFloat b
    in
    case ( polygonListPoint |> List.head, polygonListPoint |> List.tail ) of
        ( Just head, Just tail ) ->
            "M" ++ vector2String head ++ " " ++ (tail |> List.map (\point -> "L " ++ vector2String point) |> String.join " ")

        _ ->
            ""


svgCenteredText : String -> Float2 -> Float -> String -> Html.Html a
svgCenteredText text ( x, y ) fontSize color =
    Svg.text_
        ([ SA.x (x |> String.fromFloat)
         , SA.y (y |> String.fromFloat)
         ]
            ++ svgCenteredTextStyle fontSize color
        )
        [ Svg.text text ]


cssFontFamily : String
cssFontFamily =
    "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"


svgCenteredTextStyle : Float -> String -> List (Html.Attribute a)
svgCenteredTextStyle fontSize color =
    [ ( "text-anchor", "middle" )
    , ( "font-size", (fontSize |> String.fromFloat) ++ "px" )
    , ( "font-family", cssFontFamily )
    , ( "fill", color )
    , ( "opacity", "0.7" )
    ]
        |> List.map htmlAttributeStyleFromTuple


svgRectAttributesSizeAll : List (Html.Attribute a)
svgRectAttributesSizeAll =
    [ SA.width "9999", SA.height "9999" ]


styleAttributes : HtmlStyle -> List (Html.Attribute a)
styleAttributes =
    List.map htmlAttributeStyleFromTuple


htmlAttributeStyleFromTuple : ( String, String ) -> Html.Attribute a
htmlAttributeStyleFromTuple ( property, value ) =
    HA.style property value
