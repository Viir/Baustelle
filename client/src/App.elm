import Html exposing (beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes as SA
import Vector2 exposing (Float2)
import Console


main : Program Never Model Msg
main =
  beginnerProgram { model = init, view = view, update = update }

type Msg
  = SiteViewport SiteViewportMsg

type SiteViewportMsg
  = MouseEvent Console.MouseEvent
  | Error String

type alias SiteViewportModel =
  {
    mouseLocationInWorld : Maybe Float2
  }

type alias Model =
  {
    world : WorldModel,
    siteViewport : SiteViewportModel
  }

type alias WorldModel =
  {

  }

init : Model
init =
  {
    world = {},
    siteViewport = defaultSiteViewport
  }

defaultSiteViewport : SiteViewportModel
defaultSiteViewport =
  {
    mouseLocationInWorld = Nothing
  }

view : Model -> Html.Html Msg
view model =
  siteViewportView model.world model.siteViewport
  |> Html.map SiteViewport

siteViewportView : WorldModel -> SiteViewportModel -> Html.Html SiteViewportMsg
siteViewportView world viewport =
  let
    inputElement : Html.Html SiteViewportMsg
    inputElement =
      Svg.rect ([ SA.width "9999", SA.height "9999", SA.fill "transparent" ] |> List.append Console.setMouseEventAttribute) []
      |> Html.map (\maybeEvent -> maybeEvent |> Maybe.andThen (\event -> Just (MouseEvent event)) |> Maybe.withDefault (Error "mouse event"))
  in
    Svg.svg [ SA.width "400", SA.height "400", style [("background", "#101010") ]]
    [
      inputElement
    ]

update : Msg -> Model -> Model
update msg model =
  case msg of
  SiteViewport viewportMsg -> { model | siteViewport = (updateSiteViewport viewportMsg model.siteViewport) }

updateSiteViewport : SiteViewportMsg -> SiteViewportModel -> SiteViewportModel
updateSiteViewport msg viewport =
  case msg of
  MouseEvent mouseEvent ->
    { viewport | mouseLocationInWorld = Just mouseEvent.offset }
  Error error -> viewport

