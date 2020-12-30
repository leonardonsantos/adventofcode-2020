-- https://adventofcode.com/2020/day/11

import qualified Data.Map as Map

main = do
  contents <- getContents
  let myLines = lines contents
  let mInitial = parse myLines
  let myMap = Map.fromList mInitial
  let ((maxI, maxJ),_) = Map.findMax myMap
  let s1 = solve maxI maxJ myMap Map.empty
  let r1 = Map.foldr (\s acc -> if s=='#' then acc+1 else acc) 0 s1
  print r1

parse :: [[Char]] -> [((Int,Int),Char)]
parse myLines =
  let
    indexedLines = zip [0..] myLines
    indexedLinesColumns = fmap (\(i,xs) -> (zip (zip (cycle [i]) [0..]) xs)) indexedLines
  in
    concat indexedLinesColumns

solve :: Int -> Int -> Map.Map (Int,Int) Char -> Map.Map (Int,Int) Char -> Map.Map (Int,Int) Char
solve maxI maxJ current previous =
  if current == previous
    then current
    else solve maxI maxJ (f current) current
  where
    adjacents m i j =
      let
        positions = [(i-1,j-1), (i-1,j), (i-1, j+1), (i,j-1), (i, j+1),  (i+1,j-1), (i+1,j), (i+1, j+1)]
        validPositions = filter (\(i,j) -> i>=0 && j>=0 && i<=maxI && j<=maxJ ) positions
      in
        fmap (m Map.!) validPositions
    occupiedAdjacents m i j = length $ filter (=='#') $ adjacents m i j
    f m = Map.mapWithKey f' m
    f' (i,j) s =
      let
        occA = occupiedAdjacents current i j
      in
        if s == 'L' && occA == 0
          then '#'
        else if s == '#' && occA >= 4
          then 'L'
          else s
