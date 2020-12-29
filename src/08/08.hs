-- https://adventofcode.com/2020/day/8

import qualified Data.Set as Set

main = do
  contents <- getContents
  let instructions = lines contents
  let parsedInstructions = fmap parse instructions
  let r1 = process parsedInstructions
  print r1

parse :: String -> (String, Int)
parse instr =
  let
    cmd:number:_ = breakOnChars " " instr
    number' = filter (\c -> c /= '+') number
    n = read number' :: Int
  in
    (cmd,n)

breakOnChars :: [Char] -> String -> [String]
breakOnChars _ "" = []
breakOnChars chars (x:xs) =
  if (elem x chars)
    then [""] ++ (breakOnChars chars xs)
    else (
      let ys = breakOnChars chars xs
      in
        if ys == []
          then [[x]]
          else (x:(head ys)):(tail ys)
    )

process :: [(String,Int)] -> Int
process instrs = runProcess instrs 0 0 Set.empty

runProcess :: [(String,Int)] -> Int -> Int -> Set.Set Int -> Int
runProcess instrs i acc set =
  let
    (cmd,n) = instrs !! i
    set' = Set.insert i set
  in
    if Set.member i set
      then acc
    else if cmd == "acc"
      then runProcess instrs (i+1) (acc+n) set'
    else if cmd == "jmp"
      then runProcess instrs (i+n) acc set'
      else runProcess instrs (i+1) acc set'
