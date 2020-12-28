-- https://adventofcode.com/2020/day/7

import Text.Regex.PCRE
import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
  contents <- getContents
  let parsed = Map.fromList $ fmap parse (lines contents)
  let reversed = reverseMap parsed
  let rset = children "shiny gold" reversed Set.empty
  let count = Set.size $ Set.delete "shiny gold" rset
  print count

parse s =
  let
    (a,b) = splitContain s
    parsedBags = splitBagList b
  in
    (a, parsedBags)

splitContain :: String -> (String, String)
splitContain s =
  let
    patternContain = "(.*) bags contain (.*)$"
    (_, _, _, [a, b]) = s =~ patternContain :: (String, String, String, [String])
  in
    (a,b)

splitBagList :: String -> [(Int, String)]
splitBagList s =
  let
    splited = breakOnChars ",." s
    parsed = fmap parseBag splited
  in
    fmap parseNumber parsed
  where
    parseBag :: String -> (String, String)
    parseBag s =
      let
        patternBag = "(\\d+|no)\\s+(.*)\\s+bag.*"
        (_, _, _, a:b:_) = s =~ patternBag :: (String, String, String, [String])
      in
        (a,b)

    parseNumber :: (String,String) -> (Int,String)
    parseNumber (a,b) =
      if a == "no"
        then (0, b)
        else (read a :: Int, b)

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

reverseMap :: Map.Map String [(Int,String)] -> Map.Map String [(Int,String)]
reverseMap m = Map.foldrWithKey f Map.empty m
  where
    f k bags m' = foldl (\m'' (n,bag) -> Map.insertWith (++) bag [(n,k)] m'') m' bags


-- DFS
children :: String -> Map.Map String [(Int,String)] -> Set.Set String -> Set.Set String
children node m set =
  let
    set' = Set.insert node set
  in
    if (Set.notMember node set) && (Map.member node m)
      then foldl (\set'' (n,bag) -> children bag m set'') set' ((Map.!) m node)
      else set'