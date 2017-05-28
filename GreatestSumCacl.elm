
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
  List.range index (index + sumsize - 1) |>
  itemsinrange  |>
  List.map (\ (a, b)-> b ) |> 
  List.sum

diagsumitem : (Int, Int) ->  Int
diagsumitem (index, val) =
  List.range index (index + 3) |>
  diagonalindexes |>
  itemsinrange  |>
  List.map getvalue |> 
  List.sum


digitpositioncalc : (Int,  Int) -> Int
digitpositioncalc ( digitpos, index) = 
  (digitpos * cubesize)  +  index 


diagonalindexes : List Int -> List Int
diagonalindexes indexs = 
  indexs |>
  List.indexedMap (\ a b -> (a , b)) |>  
  List.map digitpositioncalc

sumsofsetsh : List (Int, Int) -> List (Int)
sumsofsetsh d =
  d |>
  List.filter (\ (index, val) ->  validcell index)  |>
  List.map rowsumitem

sumsofsetsv : List (Int, Int) -> List (Int)
sumsofsetsv d =
  d |>
  List.filter (\ (index, val) ->  validcell index)  |>
  List.map diagsumitem
--  List.map rowsumitem
 

answer : Int
answer = 
  data |>
  (\d -> List.append (sumsofsetsh d) (sumsofsetsv d) ) |> 
  List.maximum  |>
  valueorzero

main = 
  text (toString (answer))


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
         List.any(\x -> x == index)
  )

valueorzero : Maybe Int -> Int
valueorzero ans =
  case ans of
       Nothing -> 0
       Just x -> x

getvalue : (Int, Int) -> Int
getvalue (index, value) =
  if index <= (cubesize ^2) then
    value
  else
   0