import Html exposing (text)
 
data : List (Int, Int)
data = List.range 1 64 |> List.indexedMap (\ a b -> (a + 1, b))

main =
  text (toString data)