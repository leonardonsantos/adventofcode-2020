-- https://adventofcode.com/2020/day/10

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe

main = do
  contents <- getContents
  let adapters = fmap (\x -> read x :: Integer) (lines contents)
  let (diff1, diff3) = useAdaptersGreedy adapters
  print (diff1 * diff3)
  let setAdapters = Set.fromList adapters
  let r2 = allPoss setAdapters
  print r2

useAdaptersGreedy :: [Integer] -> (Integer,Integer)
useAdaptersGreedy xs = calculate (Set.fromList xs) 0 (0,0)
  where
    calculate :: Set.Set Integer -> Integer -> (Integer,Integer) -> (Integer,Integer)
    calculate mySet i (diff1, diff3) =
      if (Set.member (i+1) mySet)
        then calculate mySet (i+1) (diff1+1, diff3)
        else
          if (Set.member (i+3) mySet)
            then calculate mySet (i+3) (diff1, diff3+1)
            else (diff1, diff3+1)

{-
Dynamic Programming!
```
(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 12, 15, 16, 19, (22)
```

Observing that we can see that `7, 10, 11, 12, 15, 16, 19, (22)` appears 4 times.
We can also see that the answer for "Solve from 7" are "two possibilities".
Solve 7 = 2
Solve 6 = Solve 7 = 2
Solve 5 = (Solve 6) + (Solve 7) = 2 + 2 = 4
Solve 4 = (Solve 5) + (Solve 6) + (Solve 7) = 4 + 2 + 2 = 8
Solve 0 = Solve 1 = Solve 4 = 8

So we can keep the result of already computed partial solutions.
-}

allPoss :: Set.Set Integer -> Integer
allPoss setAdapters = fst $ solve 0 (Set.insert 0 setAdapters) Map.empty (Set.findMax setAdapters)

solve :: Integer -> Set.Set Integer -> Map.Map Integer Integer -> Integer -> (Integer, Map.Map Integer Integer)
solve n set dict stop =
  let
    (r1,d1) = try n 1 dict
    (r2,d2) = try n 2 d1
    (r3,d3) = try n 3 d2
    r = if n == stop
          then 1
          else
            if (n > stop) || (Set.null set) || (not $ Set.member n set)
              then 0
              else r1 + r2 + r3
    useThisDict = if (n == stop) || (n > stop) || (Set.null set) || (not $ Set.member n set)
                    then dict
                    else d3
    d = Map.insert n r useThisDict
  in
    if Map.member n dict
      then (dict Map.! n, dict)
      else (r, d)
  where
    try n i dict' =
      let set' = Set.delete n set
      in solve (n+i) set' dict' stop
