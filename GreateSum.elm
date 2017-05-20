import Html exposing (text)
 
cubesize : Int
cubesize = 8

sumsize : Int
sumsize = 4

vldrow : Int -> Bool
vldrow index =
  let
    remainder =(index % cubesize)
  in
    sumsize <= (cubesize - remainder)   &&  (remainder > 0)
  
data : List (Int, Int)
data = List.range 1 (cubesize^2) |> List.indexedMap (\ a b -> (a + 1, b))

rowdata : List (Int, Int)
rowdata = List.filter (\ (index, val) ->  vldrow index) data

main = 
  text (toString (rowdata))