-- https://adventofcode.com/2020/day/5

import Data.List

main = do
  contents <- getContents
  let tickets = lines contents
  let parsedTickets = fmap parse tickets
  let maxT = maxTicket parsedTickets
  print maxT
  let missingT = missingTicket parsedTickets
  print missingT

parse :: String -> (Int, Int, Int)
parse ticket =
  let
    rows = take 7 ticket
    row = calculate 0 127 rows
    columns = drop 7 ticket
    column = calculate 0 7 columns
    passId = row * 8 + column
  in (row, column, passId)
  where
    calculate :: Int -> Int -> String -> Int
    calculate a _ [] = a
    calculate a b (x:xs) =
      let
        auxMiddle = quot (a+b) 2
      in
        if (a>=b)
          then a
          else
            if (x=='F' || x=='L')
              then calculate a auxMiddle xs
              else calculate (auxMiddle+1) b xs

maxTicket :: [(Int,Int,Int)] -> Int
maxTicket tickets =
  let passIds = fmap (\(_,_,i) -> i) tickets
  in foldl1 max passIds

missingTicket :: [(Int,Int,Int)] -> Int
missingTicket tickets =
  let
    passIds = sort $ fmap (\(_,_,i) -> i) tickets
    (_, missingT) = foldl process (0,0) passIds
  in
    missingT
  where
    process (i,r) x =
      if (x==i+1)
        then (x,r)
        else (x,i+1)