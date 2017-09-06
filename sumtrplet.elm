{-  
#9
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

-}

import Html exposing (text)

sumCheck : Int
sumCheck = 1000



rangeItems : Int -> List Int
rangeItems curitem =
  List.range curitem (sumCheck - 1) 


dataFiltered : List (Int, Int) -> List (Int, Int)
dataFiltered data =
  List.filter (\(x,y) -> x + y < sumCheck ) data

dblVals :  List (Int, Int)
dblVals  = 
  let 
    curDbl = \curitem -> List.map (\x -> (curitem,x))  (rangeItems curitem)
  
  in 
   (List.map curDbl  (List.range 1 sumCheck)) |> List.concat |> dataFiltered
  

calcTrip : Int -> Int -> Int
calcTrip a b =
  (a^2 + b^2) |> toFloat |> sqrt |> round
 


isAns : (Int, Int) -> Bool
isAns  (a, b) =
  let 
    c = sumCheck - a - b
  in
    ((calcTrip a b) == c)  && 
    (c > a )  && 
    (c > b )  && 
    (c^2 == (a^2 + b^2 ))
  
runner : List (Int, Int)
runner =
  dblVals  |>  List.filter isAns 
   
calAns :  (Int, Int) -> Int
calAns  (a, b) =
  let 
    c = sumCheck - a - b
  in
    c * b * a


ans : a -> String
ans x = toString x

main =
  text ( runner  |> List.map calAns |>  ans)
