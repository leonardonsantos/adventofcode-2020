-- https://adventofcode.com/2020/day/3

main = do
  contents <- getContents
  let (pos, accTrees) = foldl resolve (0,0) (lines contents)
  print $ show $ accTrees

resolve :: (Int, Int) -> String -> (Int, Int)
resolve (current, accTrees) line = let
  nextAccTress = if (line !! current == '#') then accTrees + 1 else accTrees
  next = mod (current + 3) (length line)
  in (next, nextAccTress)