module Base exposing (..)

import Dict


dictFromListWithIndexAsKey : List a -> Dict.Dict Int a
dictFromListWithIndexAsKey list =
    list |> List.indexedMap (\index element -> (index, element)) |> Dict.fromList

dictMapValues : (a -> b) -> Dict.Dict comparable a -> Dict.Dict comparable b
dictMapValues valueMap dict = dict |> Dict.map (\_ value -> valueMap value)

withListTransformApplied : List (a -> a) -> a -> a
withListTransformApplied listTransform original =
    case listTransform |> List.head of
    Just transform -> withListTransformApplied (listTransform |> List.tail |> Maybe.withDefault []) (transform original)
    Nothing -> original
