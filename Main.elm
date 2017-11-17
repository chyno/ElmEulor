import Html exposing (text)
import Debug exposing (log)

{--


The following iterative sequence is defined for the set of positive
 integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following 
sequence:
13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1)
 contains 10 terms. Although it has not been proved yet (Collatz Problem),
  it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.

--}
 
type alias Seqcount =
 {
    val : Int
  , count : Int
 }

type alias Tally = 
 {
   seqcounts : List Seqcount
   , curmax : Int
 }

maxstart : Int
maxstart = 100

evencalc : Int -> Int
evencalc x =  (toFloat x) / 2
               |> floor

oddcalc : Int -> Int
oddcalc x = (3 * x) + 1

existingseq : Int -> List Seqcount -> Maybe Seqcount
existingseq x items = 
   List.filter (\item -> item.val == x) items |>
   List.head
      
candidatecountcalc :  List Seqcount -> Int -> Int
candidatecountcalc  items x =
     candidatecountcalcr  items x 0

candidatecountcalcr :  List Seqcount -> Int -> Int -> Int
candidatecountcalcr   items x curcount =
  if x == 1 then
    curcount
  else
    let
        maybetally = (existingseq x items)
    in
      case maybetally of
        Just t ->
           t.count
        _ ->
          let
              fnnextcount = 
                if (x % 2) == 0 then
                  (evencalc x)  
               else
                  (oddcalc x)       
          in
             (candidatecountcalcr items fnnextcount (curcount + 1))
              
tallycalc : Int -> Tally -> Tally
tallycalc x tl =
  let
    maybetally = 
    (existingseq x tl.seqcounts)
  in
    case maybetally of
      Just t ->
         tl 
      _ ->
        let
            candidatecount = (candidatecountcalc  tl.seqcounts x)
            newmax =  
              if candidatecount > tl.curmax then
                candidatecount
              else
                tl.curmax
               
        in
            
        {tl | 
          curmax = newmax,
          seqcounts = {
            val = x
            , count = candidatecount
          }::tl.seqcounts 
         }
         

 
main =
  text ( existingseq 0 []  |> toString)

  {-

findcols : List Int ->   tally  -> tally
findcols toberun curtally = 
  case toberun of
      []     ->  curtally
      hd::tl -> 
        let
           newtally = calcnewtally hd curtally
           newcanidates = newtall tl
        in
         findcols newcanidates newtally


runner : Int -> List Int -> List Int
runner x  items = 
  if x == 1 then 
    1::items
  else
    if (x % 2) == 0 then
     runner (evencalc x) (x::items) 
    else
      runner (oddcalc x) (x::items)

newcandidates : List Int -> List Int -> List Int
newcandidates canidates colseq =
  let
      tofilterout = List.filter (\x -> x <= maxnum) colseq
      filtercand = \x ->   List.member x tofilterout |> not
  in  
    List.filter filtercand canidates

calcnewtally : int ->  tally -> tally
calcnewtally candnum t  =
  let
      newseq =   
  in
       { t | seqcounts = x:seqcounts, curmax = newmax  } 


  -}