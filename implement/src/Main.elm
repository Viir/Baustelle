module Main exposing (main)

import Browser
import Browser.Events
import Game
import Html
import Random
import Time


main : Program () State Event
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Event
    = GameEvent Game.Event
    | TimeHasArrivedEvent Time.Posix


type alias State =
    { timeMilli : Int
    , game : Game.State
    }


init : () -> ( State, Cmd Event )
init _ =
    ( { timeMilli = 0
      , game = Game.initialGame
      }
    , Cmd.none
    )


subscriptions : State -> Sub Event
subscriptions _ =
    Browser.Events.onAnimationFrame TimeHasArrivedEvent


view : State -> Html.Html Event
view state =
    Game.view state.game
        |> Html.map GameEvent


update : Event -> State -> ( State, Cmd Event )
update event state =
    case event of
        GameEvent gameEvent ->
            ( { state | game = Game.update gameEvent state.game }, Cmd.none )

        TimeHasArrivedEvent time ->
            let
                timeMilli =
                    time |> Time.posixToMillis

                durationMilli =
                    timeMilli - state.timeMilli
            in
            ( { state
                | timeMilli = timeMilli
                , game = Game.progress (durationMilli |> min 1000) (Random.initialSeed (timeMilli * 347161)) state.game
              }
            , Cmd.none
            )
