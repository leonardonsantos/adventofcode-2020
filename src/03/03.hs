-- https://adventofcode.com/2020/day/3

main = do
  contents <- getContents
  let lineContents = lines contents
  let (_, accTrees) = foldl (goRight 3) (0,0) lineContents
  print $ show $ accTrees
  let (_, (_, accTress2a)) = foldl (goDown 1 1) (1, (0,0)) lineContents
--  print $ show $ accTress2a
  let (_, (_, accTress2b)) = foldl (goDown 3 1) (1, (0,0)) lineContents
--  print $ show $ accTress2b
  let (_, (_, accTress2c)) = foldl (goDown 5 1) (1, (0,0)) lineContents
--  print $ show $ accTress2c
  let (_, (_, accTress2d)) = foldl (goDown 7 1) (1, (0,0)) lineContents
--  print $ show $ accTress2d
  let (_, (_, accTress2e)) = foldl (goDown 1 2) (2, (0,0)) lineContents
--  print $ show $ accTress2e
  let result = accTress2a * accTress2b * accTress2c * accTress2d * accTress2e
  print $ show $ result

goRight :: Int -> (Int, Int) -> String -> (Int, Int)
goRight right (current, accTrees) line = let
  nextAccTress = if (line !! current == '#') then accTrees + 1 else accTrees
  next = mod (current + right) (length line)
  in (next, nextAccTress)

goDown :: Int -> Int -> (Int, (Int, Int)) -> String -> (Int, (Int, Int))
goDown right down (currentDown, (currentRight, accTrees)) line = let
  nextDown = if (currentDown - 1 <= 0) then down else currentDown - 1
  (nextRight, nextAccTrees) = goRight right (currentRight, accTrees) line
  in if (down == currentDown)
    then (nextDown , (nextRight, nextAccTrees))
    else (nextDown , (currentRight, accTrees))