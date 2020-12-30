-- https://adventofcode.com/2020/day/11

import qualified Data.Map as Map
import Data.Maybe

main = do
  contents <- getContents
  let myLines = lines contents
  let mInitial = parse myLines
  let myMap = Map.fromList mInitial
  let ((maxI, maxJ),_) = Map.findMax myMap
  let s1 = solve maxI maxJ myMap Map.empty
  putStrLn $ printMap s1
  let r1 = countOccupied s1
  print r1
  let s2 = solve2 maxI maxJ myMap Map.empty
  putStrLn $ printMap s2
  let r2 = countOccupied s2
  print r2

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

countOccupied :: Map.Map (Int,Int) Char -> Int
countOccupied m = Map.foldr (\s acc -> if s=='#' then acc+1 else acc) 0 m

printMap :: Map.Map (Int,Int) Char -> String
printMap m =
  let (_,r) = Map.foldlWithKey (\(i',acc) (i,j) c -> if i/=i' then (i,acc++"\n"++[c]) else (i,acc++[c])) (0,"") m
  in r

solve2 :: Int -> Int -> Map.Map (Int,Int) Char -> Map.Map (Int,Int) Char -> Map.Map (Int,Int) Char
solve2 maxI maxJ current previous =
  if current == previous
    then current
    else solve2 maxI maxJ (f current) current
  where
    adjacents m i j = fmap (\pos -> adjacent2 m pos maxI maxJ i j) [1..8]
    occupiedAdjacents m i j = length $ filter ((Just '#')==) $ adjacents m i j
    f m = Map.mapWithKey f' m
    f' (i,j) s =
      let
        occA = occupiedAdjacents current i j
      in
        if s == 'L' && occA == 0
          then '#'
        else if s == '#' && occA >= 5
          then 'L'
          else s

adjacent2 :: Map.Map (Int,Int) Char -> Int -> Int -> Int -> Int -> Int -> Maybe Char
adjacent2 m pos maxI maxJ i j =
  let
    (i',j') = coordPos pos i j
    val = valid i' j'
    c = m Map.! (i',j')
  in
    if pos<1 || pos>8 || (not val)
      then Nothing
      else
        if c=='.'
          then adjacent2 m pos maxI maxJ i' j'
          else Just c
  where
    valid i j = i>=0 && i<= maxI && j>=0 && j<=maxJ
    coordPos pos i j =
      if (pos==1)
        then (i-1,j-1)
      else if (pos==2)
        then (i-1,j)
      else if (pos==3)
        then (i-1,j+1)
      else if (pos==4)
        then (i,j-1)
      else if (pos==5)
        then (i,j+1)
      else if (pos==6)
        then (i+1,j-1)
      else if (pos==7)
        then (i+1,j)
      else if (pos==8)
        then (i+1,j+1)
      else (i,j)