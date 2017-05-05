module Base exposing (..)

import Dict
import Random


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

pickRandomItemFromListWeighted : Random.Seed -> List (a, Int) -> Maybe a
pickRandomItemFromListWeighted randomSeed listWeighted =
  let
    listWithBound =
        listWeighted |> List.foldl (\(item, itemWeight) listSoFar ->
            let
                lastBound =
                    listSoFar |> List.head
                    |> Maybe.andThen (\(_, lastItemBound) -> Just lastItemBound) |> Maybe.withDefault 0
            in
                (item, lastBound + itemWeight) :: listSoFar) []

    totalBound =
        listWithBound |> List.head
        |> Maybe.andThen (\(_, lastItemBound) -> Just lastItemBound) |> Maybe.withDefault 0

    (weightUnitIndex, _) = Random.step (Random.int 0 totalBound) randomSeed
  in
    listWithBound
    |> List.filter (\(_, itemBound) -> itemBound < weightUnitIndex)
    |> List.head |> Maybe.andThen (\(item, _) -> Just item)
