
import Html exposing (text)
 
cubesize : Int
cubesize = 8

sumsize : Int
sumsize = 4

data : List (Int, Int)
data = List.range 1 (cubesize^2) |> List.indexedMap (\ a b -> (a + 1, b))
 
  
--
rowsumitem : (Int, Int) ->  Int
rowsumitem (index, val) =
  List.range index (index + 4) |>
  itemsinrange  |>
  List.map (\ (a, b)-> b ) |> 
  List.sum


digitpositioncalc : Int -> Int -> Int
digitpositioncalc index digitpos = 
  (digitpos * cubesize)  +  index + digitpos


sumsofsets : List (Int, Int) -> List Int
sumsofsets d =
  d |>
  List.filter (\ (index, val) ->  validcell index)  |>
  rowsumitem
 

answer : Int
answer = 
  data
  sumsofsets |> 
  List.maximum  |>
  valueorzero

main = 
  text (toString (ans))


-- Helper methods
validcell : Int -> Bool
validcell index =
  let
    remainder = (index % cubesize)
  in
    (sumsize <= (cubesize - remainder) + 1)   &&  (remainder > 0)


itemsinrange :  List Int -> List (Int, Int)
itemsinrange items =
  data |>
  List.filter(
     \(index, value) -> 
         items |>
         List.any(\x > x = index)
  )