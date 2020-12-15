-- https://adventofcode.com/2020/day/15

import qualified Data.Map as Map

main = do
  let input = [19,0,5,1,10,13]
  let (_,_,lastX) = calculate input
  print lastX

stopPos = 2021

-- calculate :: [Int] -> Int
calculate xs =
  let
    (myMap,pos,lastX) = foldl (\(myMap,i,_) x -> (newElement myMap x i, i+1, x)) (Map.empty,1,0) xs
  in
    processNext [] myMap pos lastX
  where
    newElement :: Map.Map Int [Int] -> Int -> Int -> Map.Map Int [Int]
    newElement myMap x pos =
      let
        baseValue = if Map.member x myMap then (Map.!) myMap x else []
      in
        Map.insert x (pos:baseValue) myMap

    processNext :: [Int] -> Map.Map Int [Int] -> Int -> Int -> ([Int], Map.Map Int [Int], Int)
    processNext acc myMap pos lastX =
      let
        x = calculateX myMap lastX
        newMap = newElement myMap x pos
      in
        if pos == stopPos
          then (acc, myMap, lastX)
          else processNext (x:acc) newMap (pos+1) x
      where
        calculateX myMap lastX =
          let
            list = (Map.!) myMap lastX
            a = head list
            b = head (tail list)
          in
            if ((not (Map.member lastX myMap)) || (length list == 1))
              then 0
              else a - b

