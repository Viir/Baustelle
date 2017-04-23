module Base exposing (..)

import Dict


dictFromListWithIndexAsKey : List a -> Dict.Dict Int a
dictFromListWithIndexAsKey list =
    list |> List.indexedMap (\index element -> (index, element)) |> Dict.fromList

