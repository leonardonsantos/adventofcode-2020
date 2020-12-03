-- https://adventofcode.com/2020/day/2

import Text.Regex.Posix

main = do
  contents <- getContents
  let parsedLines = fmap parse (lines contents)
  let result = foldl (\acc (a,b,c,s) -> if (verify a b c s) then acc+1 else acc) 0 parsedLines
  print $ show $ result
  let result2 = foldl (\acc (a,b,c,s) -> if (verify2 a b c s) then acc+1 else acc) 0 parsedLines
  print $ show $ result2

parse :: String -> (Int, Int, Char, String)
parse line = let
  pattern = "^([0-9]+)-([0-9]+) (.): (.*)$"
  (_, _, _, [a, b, c, s]) = line =~ pattern :: (String, String, String, [String])
  in (read a, read b, head c, s)

verify :: Int -> Int -> Char -> String -> Bool
verify a b c s = let
  count = foldl (\acc x -> if x==c then acc+1 else acc) 0 s
  in (count >= a) && (count <= b)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

nth :: [a] -> Int -> Maybe a
nth [] _ = Nothing
nth xs i = if i < (length xs) then Just (xs !! i) else Nothing

verify2 :: Int -> Int -> Char -> String -> Bool
verify2 a b c s = let
  condA = maybe False (== c) (nth s (a-1))
  condB = maybe False (== c) (nth s (b-1))
  in xor condA condB
