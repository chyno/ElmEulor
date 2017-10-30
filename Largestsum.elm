module ProblemThirteen exposing (runner)

import Html exposing (text)
import ProblemThirteen exposing (data)
 
chunck : Int
chunck = 10

toint : String -> Int
toint val =
  String.toInt val |> Result.toMaybe |> Maybe.withDefault 0

leftoverstring : String -> String
leftoverstring x =
    String.left ((String.length x) - chunck) x

    
doneadding : List String -> Bool
doneadding items =
  let
      maybeitem = List.head items
  in
     case maybeitem of
         Just item ->
             (String.length item) < 1
         _ ->
            False  

addnum :  List String -> Int -> Int
addnum  stritems  cursum = 
  let
    toaddon =   toString cursum |>  leftoverstring |> toint 
    nextchunck = \x -> String.right chunck x |> toint    
  in     
  if (doneadding stritems) then 
    cursum
  else
    addnum   
           (List.map leftoverstring stritems)
           (List.map nextchunck stritems |> List.sum |> (+) toaddon)
 

runner :  String
runner  = 
   addnum data 0 |> toString |>  String.left 10  

main =
  text (toString  (runner))