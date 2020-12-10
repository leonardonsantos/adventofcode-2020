-- https://adventofcode.com/2020/day/10

import qualified Data.Set as Set
import Data.List

main = do
  contents <- getContents
  let adapters = fmap (\x -> read x :: Int) (lines contents)
  let (diff1, diff3) = useAdaptersGreedy adapters
  print (diff1 * diff3)
  let nAllPossibilities = countAllPossibilities adapters
  print nAllPossibilities

useAdaptersGreedy :: [Int] -> (Int,Int)
useAdaptersGreedy xs = calculate (Set.fromList xs) 0 (0,0)
  where
    calculate :: Set.Set Int -> Int -> (Int,Int) -> (Int,Int)
    calculate mySet i (diff1, diff3) =
      if (Set.member (i+1) mySet)
        then calculate mySet (i+1) (diff1+1, diff3)
        else
          if (Set.member (i+3) mySet)
            then calculate mySet (i+3) (diff1, diff3+1)
            else (diff1, diff3+1)

data Tree a = Nil | Node a [Tree a] deriving (Show, Eq)

addChild :: Tree a -> Tree a -> Tree a
addChild (Node n xs) Nil = Node n xs
addChild (Node n xs) child = Node n (child:xs)

countNodesEqual :: (Eq a) => a -> Tree a -> Int
countNodesEqual _ Nil = 0
countNodesEqual x (Node n []) =
  if (n == x)
    then 1
    else 0
countNodesEqual x (Node n children) =
  foldr (+) 0 $ fmap (countNodesEqual x) children

countAllPossibilities :: [Int] -> Int
countAllPossibilities xs =
  let
    mySet = Set.fromList xs
    t = buildTreePossibilities mySet (Node 0 [])
    myMax = Set.findMax mySet
  in
    countNodesEqual myMax t

buildTreePossibilities :: Set.Set Int -> Tree Int -> Tree Int
buildTreePossibilities _ Nil = Nil
buildTreePossibilities mySet t =
  let
    Node n children = t
    t1 = buildTreePossibilities mySet $ tryAdaptor mySet (n+1)
    t2 = buildTreePossibilities mySet $ tryAdaptor mySet (n+2)
    t3 = buildTreePossibilities mySet $ tryAdaptor mySet (n+3)
  in
    addChild (addChild (addChild t t1) t2) t3
  where
    tryAdaptor mySet n =
      if (Set.member n mySet)
        then Node n []
        else Nil

