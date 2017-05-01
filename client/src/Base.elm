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

listRandomItem : Random.Seed -> List a -> Maybe a
listRandomItem randomSeed list =
  let
      (index, _) = Random.step (Random.int 0 ((list |> List.length) - 1)) randomSeed
  in
    list |> List.drop index |> List.head
