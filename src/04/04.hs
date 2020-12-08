-- https://adventofcode.com/2020/day/4

import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
  contents <- getContents
  let records = breakOnEmptyLine contents
  let parsedRecords = fmap (\r -> parseRecord r) records
  let validatedRecords = fmap validate parsedRecords
  let countValidated = foldl (\acc x -> if x then acc+1 else acc) 0 validatedRecords
  print countValidated
  let validatedRecords2 = fmap validate2 parsedRecords
  let countValidated2 = foldl (\acc x -> if x then acc+1 else acc) 0 validatedRecords2
  print countValidated2

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

allFromListChar :: String -> String -> Bool
allFromListChar c s = let
  digits = Set.fromList c
  in foldl (&&) True $ fmap (\x -> Set.member x digits) s

allDigits :: String -> Bool
allDigits = allFromListChar "0123456789"

validateNumber :: String -> String -> String -> Bool
validateNumber s a b =
  (allDigits s)
  && (s >= a)
  && (s <= b)

validateYear :: String -> String -> String -> Bool
validateYear s a b =
  (length s <=4)
  && (validateNumber s a b)

validateHeight :: String -> Bool
validateHeight s =
  let
    heightUnit = drop (length s - 2) s
    numberHeight = take (length s - 2) s
  in
    if heightUnit == "cm"
      then validateNumber numberHeight "150" "193"
      else
        if heightUnit == "in"
          then validateNumber numberHeight "59" "76"
          else False

validateHcl :: String -> Bool
validateHcl s =
  let
    x:xs = s
  in
    (x == '#')
    && (allFromListChar "0123456789abcdef" xs)

validateEcl :: String -> Bool
validateEcl s = elem s ["amb","blu","brn","gry","grn","hzl","oth"]

validatePid :: String -> Bool
validatePid s =
  (length s == 9)
  && (allDigits s)

validate2 parsedRecord = let
  myDict = Map.fromList parsedRecord
  presentFields = fmap (\x -> Map.member x myDict) ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
  in foldl (&&) True presentFields
    && (validateYear (myDict Map.! "byr") "1920" "2002")
    && (validateYear (myDict Map.! "iyr") "2010" "2020")
    && (validateYear (myDict Map.! "eyr") "2020" "2030")
    && (validateHeight (myDict Map.! "hgt"))
    && (validateHcl (myDict Map.! "hcl"))
    && (validateEcl (myDict Map.! "ecl"))
    && (validatePid (myDict Map.! "pid"))
