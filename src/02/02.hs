-- https://adventofcode.com/2020/day/2

import Text.Regex.Posix

main = do
  contents <- getContents
  let parsedLines = fmap parse (lines contents)
  let result = foldl (\acc (a,b,c,s) -> if (verify a b c s) then acc+1 else acc) 0 parsedLines
  putStr $ show $ result

parse :: String -> (Int, Int, Char, String)
parse line = let
  pattern = "^([0-9]+)-([0-9]+) (.): (.*)$"
  (_, _, _, [a, b, c, s]) = line =~ pattern :: (String, String, String, [String])
  in (read a, read b, head c, s)

verify :: Int -> Int -> Char -> String -> Bool
verify a b c s = let
  count = foldl (\acc x -> if x==c then acc+1 else acc) 0 s
  in (count >= a) && (count <= b)