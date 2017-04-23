module Base exposing (..)

import Dict


dictFromListWithIndexAsKey : List a -> Dict.Dict Int a
dictFromListWithIndexAsKey list =
    list |> List.indexedMap (\index element -> (index, element)) |> Dict.fromList

withListTransformApplied : List (a -> a) -> a -> a
withListTransformApplied listTransform original =
    case listTransform |> List.head of
    Just transform -> withListTransformApplied (listTransform |> List.tail |> Maybe.withDefault []) (transform original)
    Nothing -> original
