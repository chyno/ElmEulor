import Html exposing (text)
import Array

type alias Cell = 
  {
      index : Int,
      col : Int,
      val : Int
  }

cubesize : Int
cubesize = 6

sumsize : Int
sumsize = 4

first : List Cell -> Cell
first items = 
  case List.head items of
    Nothing -> Cell 0 0 0
    Just a -> a


second : List Cell -> Cell
second items = 
  case  items of
    a::b::lst -> b
    _ -> Cell 0 0 0

third : List Cell -> Cell
third items = 
  case  items of
    x::a::b::lst -> b
    _ -> Cell 0 0 0

getRecords : List (Int,Int, Int) -> List Cell
getRecords items =
  List.map (\ (a, b, c) -> Cell (a + 1) (b + 1) c ) items

positioncalc : Int -> Int -> (Int,Int, Int)
positioncalc index value =
  (index, index % cubesize, value)

data : List Cell
data =  List.indexedMap positioncalc  (List.range 1 36)  |> getRecords
  
--  List.filter (\(indx,mod, c) -> mod == frst && (index < 1) ) items

sumrowvalues : Cell  -> Int
sumrowvalues cll = 
  let 
    itemstosum = List.filter (\x -> x.index >= cll.index && x.index < (cll.index + sumsize)) data
  in
    List.map (\y -> y.val)  itemstosum |> List.sum

getrowcells : Cell  -> List Int
getrowcells cell   = 
  List.filter (\x ->  (x.index >= cell.index) &&  (x.col <= (cubesize - sumsize))) data
  |> List.map sumrowvalues

  


main =
  text (toString (first data |> getrowcells))