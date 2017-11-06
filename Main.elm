import Html exposing (text)


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

evencalc : Int -> Int
evencalc x =  (toFloat x) / 2
               |> floor

oddcalc : Int -> Int
oddcalc x = (3 * x) + 1

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
      filtercand = \x ->   List.member x colseq |> not
  in
      List.filter filtercand canidates

findcols :   Int -> List Int  -> Int
findcols  curmax canidates = 
  case canidates of
      []     ->  curmax
      hd::tl -> 
        let
            colItems = (runner hd [])
            itemCount  =  List.length colItems
            newMax =
               if itemCount > curmax then
                 itemCount
              else
                curmax
        in
          findcols newMax (newcandidates tl colItems)
 

main =
  text ( findcols 0 (List.range 1 100000)  |> toString)