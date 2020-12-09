-- https://adventofcode.com/2020/day/9

import qualified Data.Set as Set

main = do
  contents <- getContents
  let numbers = fmap (\x -> read x :: Int) (lines contents)
  let (result, position) = findNumberNotCompliant 25 numbers
  print result
  let previous = take position numbers
  let result2 = contiguousThatSum result previous
  print result2
  let a = foldr1 min result2
  let b = foldr1 max result2
  print (a+b)

-- if not found, then empty list
pairSum :: (Num a, Eq a, Ord a) => [a] -> a -> [(a,a)]
pairSum [] _ = []
pairSum (x:[]) _ = []
pairSum xs sum = let
  mySet = Set.fromList xs
  in take 1 [(x,y) | x<-xs, let y = sum - x, Set.member y mySet]

findNumberNotCompliant :: Int -> [Int] -> (Int, Int)
findNumberNotCompliant preambleSize xs =
  let
    (_, result, position) = foldl check ([],[],0) xs
  in
    head result
  where
    check :: ([Int],[(Int,Int)],Int) -> Int -> ([Int],[(Int,Int)],Int)
    check (preamble, acc, position) x =
      if (length preamble < preambleSize)
        then (x:preamble, acc, position+1)
        else
          if (length (pairSum preamble x) > 0)
            then (take preambleSize (x:preamble), acc, position+1)
            else (take preambleSize (x:preamble), acc ++ [(x, position)], position+1)

contiguousThatSum :: Int -> [Int] -> [Int]
contiguousThatSum _ [] = []
contiguousThatSum s (x:xs) =
  let
    r = checkSum s xs [x]
  in
    if (length r > 1)
      then r
      else contiguousThatSum s xs
  where
    checkSum :: Int -> [Int] -> [Int] -> [Int]
    checkSum s [] _ = []
    checkSum s (y:ys) acc =
      let
        left = s - (sum (y:acc))
      in
        if (left == 0)
          then y:acc
          else
            if (left <= 0)
              then []
              else checkSum s ys (y:acc)
