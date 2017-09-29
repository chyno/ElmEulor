import Html exposing (text)
import Touple
 
allFactors : Int ->  Int -> List Int ->  List Int
allFactors basenum  testnum factors = 
  let
      lower = basenum // testnum
  in
      
  if lower >= testnum then
    factors
  else
    if (isDivisor basenum testnum) then
      (allFactors basenum  (testnum - 1) (lower::(testnum::factors)))
    else
      (allFactors basenum  (testnum - 1) factors)
 
factorCount : Int ->  Int
factorCount num =
  let
    factors = allFactors num  num []    
  in
     List.length factors 
  

isDivisor : Int -> Int -> Bool
isDivisor base numToCheck =
  (rem base numToCheck) == 0

divisorCount : Int -> Int -> Int -> Int
divisorCount nfactors nthTriangle previoussum =
  let
      tnum =  nthTriangle +  previoussum
  in
    if (factorCount tnum) > nfactors then
      tnum
    else
      divisorCount nfactors (nthTriangle + 1) tnum


runner : Int -> Int
runner nfactors = 
  divisorCount nfactors 1 0

endfactorcount : Int
endfactorcount = 6

triagfactcount : Int -> Int ->  List (Int, Int) -> Int
triagfactcount cval ccount items = 
  let
      res = 
        List.filter
           /(val, numfac) -> 
             val == cval     
  in
     if (cval == )
     case res of  
       just v

runnerv2 : Int -> Int -> List (Int, Int) -> Int
runnerv2 nthtrinum prevtriagnum prevTriagsAndCounts =
  let
      factval = prevtriagnum + nthtrinum
      nthtragcount = triagfactcount factval prevTriagsAndCounts
      
      triagnumfactcount = (factval,nthtragcount )
  in
    if (nthtragcount ==  endfactorcount) then
      factval
    else 
      (runnerv2 (nthtrinum + 1) factval (triagnumfactcount::prevTriagsAndCounts))
     
      

main =
  text (toString  (allFactors 9000 9000 []))