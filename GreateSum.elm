import Html exposing (text)
 
cubesize : Int
cubesize = 8

sumsize : Int
sumsize = 4

validcell : Int -> Bool
validcell index =
  let
    remainder =(index % cubesize)
  in
    (sumsize <= (cubesize - remainder) + 1)   &&  (remainder > 0)
  
data : List (Int, Int)
data = List.range 1 (cubesize^2) |> List.indexedMap (\ a b -> (a + 1, b))

rowsumitem : (Int, Int) ->  Int
rowsumitem (index, val) =
  data |>
  List.filter (\ (a, b) -> a >= index && a < (index + 4) )  |>
  List.map (\ (a, b)-> b ) |> 
  List.sum


filterdata : List Int
filterdata =
  data |>
  List.filter (\ (index, val) ->  validcell index)  |>
  List.map rowsumitem

answer : Int
answer = 
  filterdata |> 
  List.maximum  |>
  (\ ans ->
     case ans of
       Nothing -> 0
       Just x -> x
  )

main = 
  text (toString (answer))