-- https://adventofcode.com/2020/day/1

main = do
  contents <- getContents
  let pairs = pairSum (numbers contents) 2020
  let result = fmap (\(a,b) -> a*b) pairs
  -- putStr $ show $ result
  mapM_ print result

numbers :: String -> [Int]
numbers input =
  let allLines = lines input
      allNumbers = fmap (\s -> read s) allLines
  in allNumbers

-- if not found, then empty list
pairSum :: (Num a, Eq a) => [a] -> a -> [(a,a)]
pairSum [] _ = []
pairSum (x:[]) _ = []
pairSum xs sum = take 1 [(x,y) | x<-xs, let y = sum - x, elem y xs]

