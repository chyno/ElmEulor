import Html exposing (text)
 
cubesize : Int
cubesize = 8

sumsize : Int
sumsize = 4

valueorzero : Maybe Int -> Int
valueorzero ans =
  case ans of
       Nothing -> 0
       Just x -> x

 
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

diagsumitem : (Int, Int) -> Int
diagsumitem (index, val) =
  42

digitpositioncalc : Int -> Int -> Int
digitpositioncalc index digitpos = 
  (digitpos * cubesize)  +  index + digitpos

diagonalindexes : Int -> List Int
diagonalindexes index = 
  let
    getnext  =
      List.range index (index + 4)  |>
      List.map (\x -> x == ( digitpositioncalc index x))  |>
      valueorzero
  in
    (getnext 0) :: (getnext 1) :: (getnext 2) ::(getnext 3)

    

sums : List (Int, Int) -> List Int
sums d =
  d |>
  List.filter (\ (index, val) ->  validcell index)  |>
  (\fltlist -> (List.map rowsumitem fltlist) :: (List.map diagsumitem fltlist) )


answer : Int
answer = 
  data
  sums |> 
  List.maximum  |>
  valueorzero

main = 
  text (toString (diagonalindexes 1))