-- https://adventofcode.com/2020/day/14

import qualified Data.Map as Map
import Text.Regex.PCRE

main = do
  contents <- getContents
  let commands = lines contents
  let myMemory = processCommands commands
  let r = foldl (\acc (a,b) -> acc + b) 0 (Map.toList myMemory)
  print r

intToStringBit :: Int -> Int -> String
intToStringBit 0 _ = ""
intToStringBit i n =
  let
    bit = mod n 2
    bitString = if bit == 0 then "0" else "1"
    nLeft = quot n 2
  in
    (intToStringBit (i-1) nLeft) ++ bitString

stringBitToInt :: String -> Int
stringBitToInt str =
  let
    stringR = reverse str
  in
    calculate stringR 0
  where
    calculate :: String -> Int -> Int
    calculate [] _ = 0
    calculate (x:xs) i =
      let
        n = if (x=='1') then 2 ^ i else 0
      in
        n + (calculate xs (i+1))

applyMask :: String -> String -> String
applyMask n mask = fmap (calc) $ zip n mask
  where
    calc (a,'X') = a
    calc (_,b) = b

processCommands :: [String] -> Map.Map Int Int
processCommands commands =
  let
    (myMap, mask) = foldl processCommand (Map.empty, "") commands
  in
    myMap
  where
    regexMask = "mask = ([X01]+)" ::String
    regexValue = "mem\\[(\\d+)\\] = (\\d+)" :: String

    getMaskValue :: String -> String
    getMaskValue l =
      let
        (_,_,_,m:_) = l =~ regexMask :: (String,String,String,[String])
      in
        m

    getValue :: String -> (Int, Int)
    getValue l =
      let
        (_,_,_,matchList) = l =~ regexValue :: (String,String,String,[String])
        memPos = read (head matchList) :: Int
        value = read (head (tail matchList)) :: Int
      in
        (memPos,value)

    processCommand :: (Map.Map Int Int, String) -> String -> (Map.Map Int Int, String)
    processCommand (myMap, mask) l =
      let
        m = getMaskValue l
        (memPos,value) = getValue l
        newValue = stringBitToInt $ applyMask (intToStringBit 36 value) mask
        newMap = Map.insert memPos newValue myMap
      in
        if l =~ regexMask
          then (myMap, m)
          else (newMap, mask)


