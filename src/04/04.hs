-- https://adventofcode.com/2020/day/4

import qualified Data.Map as Map

main = do
  contents <- getContents
  let records = breakOnEmptyLine contents
  let parsedRecords = fmap (\r -> parseRecord r) records
  let validatedRecords = fmap validate parsedRecords
  let countValidated = foldl (\acc x -> if x then acc+1 else acc) 0 validatedRecords
  print countValidated

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

parseRecord :: String -> [(String, String)]
parseRecord s = fmap aux $ breakOnChars " \n" s
  where
    aux s = let
      k:v:_ = breakOnChars ":" s
      in (k,v)

validate :: [(String, String)] -> Bool
validate parsedRecord = let
  myDict = Map.fromList parsedRecord
  presentFields = fmap (\x -> Map.member x myDict) ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
  in foldl (&&) True presentFields