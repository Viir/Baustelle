module Console exposing (MouseEvent, MouseButtonType (..), MouseEventType (..), eventMouseMoveAttribute, setMouseEventAttribute, setMouseEventAttributeWithOffsetMapped)

import Vector2 exposing (Float2)
import Json.Decode as Decode
import Html exposing (Html)
import Html.Events exposing (on, onWithOptions)
import Dict


type alias RecordFloat2 = { x : Float, y : Float }

type alias MouseEventRaw =
  {
    eventType : String,
    offsetX : Float,
    offsetY : Float,
    button : Int,
    wheelDeltaX : Maybe Float,
    wheelDeltaY : Maybe Float
  }

type alias MouseEvent =
  {
    eventType : MouseEventType,
    offset : Float2,
    button : MouseButtonType,
    wheelDelta : Float2
  }

type MouseButtonType
  = Left
  | Right
  | Middle

type MouseEventType
  = MouseDown
  | MouseUp
  | MouseMove
  | MouseWheel
  | Contextmenu
  | Other


recordToFloat2 : RecordFloat2 -> Float2
recordToFloat2 record = (record.x, record.y)

eventMouseMoveAttribute : Html.Attribute Float2
eventMouseMoveAttribute =
  on "mousemove" (Decode.map recordToFloat2 mouseEventOffsetDecoder)

setMouseEventAttribute : List (Html.Attribute (Maybe MouseEvent))
setMouseEventAttribute = setMouseEventAttributeWithOffsetMapped identity

setMouseEventAttributeWithOffsetMapped : (Float2 -> Float2) -> List (Html.Attribute (Maybe MouseEvent))
setMouseEventAttributeWithOffsetMapped offsetMap =
    mouseEventTypeParseDict |> Dict.keys
    |> List.map (\event -> on event (mouseEventDecoder
      |> Decode.map mouseEventParsed
      |> Decode.map (Maybe.andThen (\event -> Just (mouseEventOffsetMapped offsetMap event)))))

mouseEventTypeParseDict : Dict.Dict String MouseEventType
mouseEventTypeParseDict =
  [ ("mousedown", MouseDown), ("mouseup", MouseUp), ("mousemove", MouseMove), ("contextmenu", Contextmenu), ("mousewheel", MouseWheel) ] |> Dict.fromList

mouseEventButtonParseDict : Dict.Dict Int MouseButtonType
mouseEventButtonParseDict =
  [(0, Left),(2, Right), (1, Middle)] |> Dict.fromList

mouseEventParsed : MouseEventRaw -> Maybe MouseEvent
mouseEventParsed mouseEventRaw =
  case (mouseEventTypeParseDict |> Dict.get mouseEventRaw.eventType, mouseEventButtonParseDict |> Dict.get mouseEventRaw.button) of
  (Just eventType, Just buttonType) -> Just
    {
      eventType = eventType,
      offset = (mouseEventRaw.offsetX, mouseEventRaw.offsetY),
      button = buttonType,
      wheelDelta = (mouseEventRaw.wheelDeltaX |> Maybe.withDefault 0, mouseEventRaw.wheelDeltaY |> Maybe.withDefault 0)
    }
  _ -> Nothing

mouseEventOffsetMapped : (Float2 -> Float2) -> MouseEvent -> MouseEvent
mouseEventOffsetMapped offsetMap event =
  { event | offset = offsetMap event.offset }

mouseEventDecoder : Decode.Decoder MouseEventRaw
mouseEventDecoder =
  Decode.map6 MouseEventRaw
    (Decode.at ["type"] Decode.string)
    (Decode.at ["offsetX"] Decode.float)
    (Decode.at ["offsetY"] Decode.float)
    (Decode.at ["button"] Decode.int)
    (Decode.at ["wheelDeltaX"] Decode.float |> Decode.maybe)
    (Decode.at ["wheelDeltaY"] Decode.float |> Decode.maybe)

mouseEventOffsetDecoder : Decode.Decoder RecordFloat2
mouseEventOffsetDecoder =
  Decode.map2 RecordFloat2
    (Decode.at ["offsetX"] Decode.float)
    (Decode.at ["offsetY"] Decode.float)
