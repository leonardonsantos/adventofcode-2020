-- https://adventofcode.com/2020/day/10

import qualified Data.Set as Set

main = do
  contents <- getContents
  let adapters = fmap (\x -> read x :: Int) (lines contents)
  let (diff1, diff3) = useAdaptersGreedy adapters
  print (diff1 * diff3)
  let allPossibilities = useAdaptersAllPossibilities adapters
--  print allPossibilities
  print $ length allPossibilities

useAdaptersGreedy :: [Int] -> (Int,Int)
useAdaptersGreedy xs = calculate (Set.fromList xs) 0 (0,0)
  where
    calculate :: Set.Set Int -> Int -> (Int,Int) -> (Int,Int)
    calculate mySet i (diff1, diff3) =
      if (Set.member (i+1) mySet)
        then calculate mySet (i+1) (diff1+1, diff3)
        else
          if (Set.member (i+3) mySet)
            then calculate mySet (i+3) (diff1, diff3+1)
            else (diff1, diff3+1)

useAdaptersAllPossibilities :: [Int] -> [[Int]]
useAdaptersAllPossibilities xs =
  let
    mySet = Set.fromList xs
    possibilities = Set.toList $ calculate mySet (Set.fromList [[0]])
    myMax = Set.findMax mySet
  in
    filter (\ (x:xs) -> x==myMax) possibilities
  where
    calculate :: Set.Set Int -> Set.Set [Int] -> Set.Set [Int]
    calculate mySet xss =
      let
        newXss = calculate' mySet xss
      in
        if (newXss == xss)
          then newXss
          else calculate mySet newXss

    calculate' :: Set.Set Int -> Set.Set [Int] -> Set.Set [Int]
    calculate' mySet xss = Set.foldl func Set.empty xss
      where
        func :: Set.Set [Int] -> [Int] -> Set.Set [Int]
        func acc xs =
          let
            i = head xs
            diff1 = if (Set.member (i+1) mySet)
                      then Set.fromList [(i+1):xs]
                      else Set.fromList [xs]
            diff2 = if (Set.member (i+2) mySet)
                      then Set.fromList [(i+2):xs]
                      else Set.fromList [xs]
            diff3 = if (Set.member (i+3) mySet)
                      then Set.fromList [(i+3):xs]
                      else Set.fromList [xs]
          in
            acc `Set.union` diff1 `Set.union` diff2 `Set.union` diff3
