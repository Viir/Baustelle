import Base exposing (..)
import Html exposing (beginnerProgram, div, button, text)
import Scenario
import ScenarioViewport


main : Program Never Model Msg
main =
  beginnerProgram { model = init, view = view, update = update }

type Msg
  = SiteViewport ScenarioViewport.Msg

type alias Model =
  {
    scenario : Scenario.Model,
    viewport : ScenarioViewport.Model
  }

init : Model
init =
  {
    scenario =
    {
      supportJoints = [ (100,100), (300,100) ] |> dictFromListWithIndexAsKey
    },
    viewport = ScenarioViewport.defaultViewport
  }

view : Model -> Html.Html Msg
view model =
  ScenarioViewport.view model.scenario model.viewport
  |> Html.map SiteViewport

update : Msg -> Model -> Model
update msg model =
  case msg of
  SiteViewport viewportMsg -> { model | viewport = (ScenarioViewport.update viewportMsg model.scenario model.viewport) }

