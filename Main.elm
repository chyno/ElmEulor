import Html exposing (text)
--import Largestsum exposing (runner)
{-


The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

-}
maxprime : Int
maxprime = 100000

addnewnumbestoskip : List Int -> Int -> List Int
addnewnumbestoskip curnumbs primenum fact =
  let
      primenumfact = primenum * fact
  in
    
  if (primenumfact >= maxprime) then
    curnumbs
  else
    addnewnumbestoskip primenumfact::curnumbs primenum  (fact + 1)

getnextnumber : List Int -> Int -> Int
getnextnumber toskip val =
  42

isnotprime : List Int -> List Int -> Int -> Bool
isnotprime primes numstoskip  num = 
  let
      isdiv = \x -> ((num % x) == 0)
  in
     List.member num numstoskip &&
     List.any isdiv primes 

startprimes : List Int
startprimes = [2,3,7]

primeslist  : List Int ->List Int -> Int -> List Int
primeslist primes numberstoskip num = 
  let
      nextnumber = getnextnumber numberstoskip num
  in

  if num >= maxprime then
    primes
  else
    if (isnotprime primes numberstoskip num) then
      primeslist  primes numberstoskip nextnumber
    else
      primeslist  (num::primes) (addnewnumbestoskip numbersstoskip num) nextnumber
      

main =
  text (toString  ( primeslist [2] 3))