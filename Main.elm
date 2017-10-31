import Html exposing (text)
import Lib exposing (..)
--import Largestsum exposing (runner)
{-


The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}

 

data : List Int
data = List.range 2 100000

              
newcanidates : Int -> List Int -> List Int -> List Int
newcanidates newprime primes canidates =
  let
      prmtest =  
        if (List.length primes) > 0 then
          primes
        else [1]
  in
    List.filter (\x -> (x % newprime) > 0) canidates 
  
 
findprimes :  List Int -> List Int  -> Trampoline List Int
findprimes  primes canidates = 
  let
      nextnum = nextval canidates
    
  in
    
  if (List.isEmpty canidates) then
    Debug.log "Done!!" 
    primes
  else
    Debug.log ("****Found Prime!! - " ++   (toString nextnum))
    --Debug.log ("Canidate Count : " ++   (toString (nextcands |> List.length)))
    findprimes  (nextnum::primes) (newcanidates nextnum primes canidates)
    
 
main =
  text (toString  ( findprimes [] data ))