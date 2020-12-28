-- https://adventofcode.com/2020/day/25

main = do
  let r1 = calculate1 10212254 12577395
  print r1

calculate1 :: Int -> Int -> Int
calculate1 cardPK doorPK =
  let
    cardLS = loopSize 1 1 cardPK
    key = encryptionKey 1 1 cardLS doorPK
  in
    key
  where
    loopSize i n key =
      let
        n' = n * 7
        n'' = mod n' 20201227
      in
        if (n'' == key)
          then i
          else loopSize (i+1) n'' key

    encryptionKey i n loopSize subjectNumber =
      let
        n' = n * subjectNumber
        n'' = mod n' 20201227
      in
        if (i >= loopSize)
          then n''
          else encryptionKey (i+1) n'' loopSize subjectNumber
