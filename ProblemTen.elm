module ProblemTen exposing (runner)

import Lib exposing (..)
import Trampoline exposing (..)
--import Largestsum exposing (runner)
{-
Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}

 
data : List Int
data = List.range 2 2000000
        
newcanidates : Int -> List Int  -> List Int
newcanidates newprime  canidates =
  List.filter (\x -> (x % newprime) > 0) canidates 
  
 
findprimes :  List Int -> List Int  -> Trampoline (List Int)
findprimes  primes canidates = 
  case canidates of
      []     -> done primes
      hd::tl -> jump (\_ 
        ->
           if (hd >= 1415) then
             done (List.concat [primes, canidates])
           else
            findprimes  (hd::primes) (newcanidates hd  canidates)
      )

runner : String
runner =
  toString (
             Trampoline.evaluate (findprimes [] data )
             |> List.sum
  )