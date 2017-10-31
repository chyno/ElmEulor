module Lib exposing (..)

nextval : List Int -> Int
nextval vals = 
  let
      maybeval = List.head vals
  in
      case maybeval of
          Just v ->
            v   
          _ ->
            0