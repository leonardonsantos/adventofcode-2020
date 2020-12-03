-- https://adventofcode.com/2020/day/1

main = do
  contents <- getContents
  let pairs = pairSum (numbers contents) 2020
  let resultPair = fmap (\(a,b) -> a*b) pairs
  putStr $ show $ resultPair
  let triples = tripleSum (numbers contents) 2020
  let resultTriples = fmap (\(a,b,c) -> a*b*c) triples
  putStr $ show $ resultTriples

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

tripleSum :: (Num a, Eq a) => [a] -> a -> [(a,a,a)]
tripleSum [] _ = []
tripleSum (x:[]) _ = []
tripleSum xs sum = take 1 [(x,y,z) | x<-xs, y<-xs, let z = sum - x - y, elem z xs]
