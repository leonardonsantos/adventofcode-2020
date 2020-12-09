-- https://adventofcode.com/2020/day/9

import qualified Data.Set as Set

main = do
  contents <- getContents
  let numbers = fmap (\x -> read x :: Int) (lines contents)
  let result = findNumbersNotCompliant 25 numbers
  print result

-- if not found, then empty list
pairSum :: (Num a, Eq a, Ord a) => [a] -> a -> [(a,a)]
pairSum [] _ = []
pairSum (x:[]) _ = []
pairSum xs sum = let
  mySet = Set.fromList xs
  in take 1 [(x,y) | x<-xs, let y = sum - x, Set.member y mySet]

findNumbersNotCompliant :: Int -> [Int] -> Int
findNumbersNotCompliant preambleSize xs =
  let
    (_, result) = foldl check ([],[]) xs
  in
    head result
  where
    check :: ([Int],[Int]) -> Int -> ([Int],[Int])
    check (preamble, acc) x =
      if (length preamble < preambleSize)
        then (x:preamble, acc)
        else
          if (length (pairSum preamble x) > 0)
            then (take preambleSize (x:preamble), acc)
            else (take preambleSize (x:preamble), acc ++ [x])