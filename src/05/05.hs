-- https://adventofcode.com/2020/day/5

main = do
  contents <- getContents
  let passports = lines contents
  let parsedPassports = fmap parse passports
  let maxP = maxPassport parsedPassports
  print maxP

parse :: String -> (Int, Int, Int)
parse passport =
  let
    rows = take 7 passport
    row = calculate 0 127 rows
    columns = drop 7 passport
    column = calculate 0 7 columns
    passId = row * 8 + column
  in (row, column, passId)
  where
    calculate :: Int -> Int -> String -> Int
    calculate a _ [] = a
    calculate a b (x:xs) =
      let
        auxMiddle = quot (a+b) 2
      in
        if (a>=b)
          then a
          else
            if (x=='F' || x=='L')
              then calculate a auxMiddle xs
              else calculate (auxMiddle+1) b xs

maxPassport :: [(Int,Int,Int)] -> Int
maxPassport passports =
  let passIds = fmap (\(_,_,i) -> i) passports
  in foldl1 max passIds