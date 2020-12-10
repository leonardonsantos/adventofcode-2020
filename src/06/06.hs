-- https://adventofcode.com/2020/day/6

import qualified Data.Set as Set

main = do
  contents <- getContents
  let answers = parse contents
  let c = countAnswers answers
  print c

breakOnEmptyLine :: String -> [String]
breakOnEmptyLine "" = []
breakOnEmptyLine (x1:x2:xs) =
  if (x1=='\n') && (x2=='\n')
    then [""] ++ (breakOnEmptyLine xs)
    else (
      let y:ys = breakOnEmptyLine (x2:xs)
      in (x1:y):ys
    )
-- last guard condition
breakOnEmptyLine xs = [xs]

parse :: String -> [String]
parse contents =
  let
    groups = breakOnEmptyLine contents
  in
    fmap (\xs -> filter (\x -> x >='a' && x<='z') xs) groups

countAnswers :: [String] -> Int
countAnswers answers =
  let
    sets = fmap Set.fromList answers
    counts = fmap Set.size sets
  in
    foldl (+) 0 counts
