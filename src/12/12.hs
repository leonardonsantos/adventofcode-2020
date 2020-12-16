-- https://adventofcode.com/2020/day/12

main = do
  contents <- getContents
  let commands = lines contents
  let parsedCommands = fmap parse commands
  let (direction, x, y) = executeCommands parsedCommands (0,0,0)
  print (direction, x, y)
  print $ (abs x) + (abs y)

parse :: String -> (Char, Int)
parse c =
  let
    letter = head c
    number = read $ tail c :: Int
  in
    (letter, number)

-- TODO: these should be Float number
executeCommands :: [(Char,Int)] -> (Int, Int, Int) -> (Int, Int, Int)
executeCommands [] curr = curr
executeCommands ((letter,number):xs) (d, x, y) =
  let
    (xN, yN)= (x, y+number)
    (xS, yS)= (x, y-number)
    (xE, yE)= (x+number, y)
    (xW, yW)= (x-number, y)
    dL = addDegrees d number
    dR = addDegrees d (-1*number)
    xF = if d == 0
           then x+number
         else if d == 90
           then x
         else if d == 180
           then x-number
         else if d == 270
           then x
           else 9999999 --something unexpected
    yF = if d == 0
           then y
         else if d == 90
           then y+number
         else if d == 180
           then y
         else if d == 270
           then y-number
           else 9999999 --something unexpected
  in
    if (letter=='N')
      then executeCommands xs (d,xN,yN)
    else if (letter=='S')
      then executeCommands xs (d,xS,yS)
    else if (letter=='E')
      then executeCommands xs (d,xE,yE)
    else if (letter=='W')
      then executeCommands xs (d,xW,yW)
    else if (letter=='L')
      then executeCommands xs (dL,x,y)
    else if (letter=='R')
      then executeCommands xs (dR,x,y)
    else if (letter=='F')
      then executeCommands xs (d,xF,yF)
    else executeCommands xs (d,x,y)
  where
    addDegrees a b =
      let
        s = a + b
      in
        if s >= 360
          then s - 360
          else
            if s < 0
              then 360 + s
              else s



