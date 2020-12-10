-- https://adventofcode.com/2020/day/10

import qualified Data.Set as Set

main = do
  contents <- getContents
  let adapters = fmap (\x -> read x :: Int) (lines contents)
  let (diff1, diff3) = useAdapters adapters
  print (diff1 * diff3)

useAdapters :: [Int] -> (Int,Int)
useAdapters xs = calculate (Set.fromList xs) 0 (0,0)
  where
    calculate :: Set.Set Int -> Int -> (Int,Int) -> (Int,Int)
    calculate mySet i (diff1, diff3) =
      if (Set.member (i+1) mySet)
        then calculate mySet (i+1) (diff1+1, diff3)
        else
          if (Set.member (i+3) mySet)
            then calculate mySet (i+3) (diff1, diff3+1)
            else (diff1, diff3+1)

