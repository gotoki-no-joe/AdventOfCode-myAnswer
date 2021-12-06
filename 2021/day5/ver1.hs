import Data.Char
import Data.Array
import Data.List

main1 fn = do
  co <- readFile fn
  let dat = map parseLine $ lines co
  print $ compute1 dat

type Point = (Int,Int)

parseLine :: String -> (Point,Point)
parseLine xs = ((read xs1, read xs3),(read xs5, read xs7))
  where
    (xs1,xs2) = span isDigit xs
    (xs3,xs4) = span isDigit $ tail xs2
    (xs5,xs6) = span isDigit $ drop 4 xs4
    (xs7,_  ) = span isDigit $ tail xs6

isHorizontal ((_,y),(_,w)) = y == w
isVertical ((x,_),(z,_)) = x == z

makeLine :: (Point,Point) -> [Point]
makeLine pp@((x,y),(z,w))
  | isHorizontal pp = [(i,y) | i <- [min x z..max x z]]
  | isVertical   pp = [(x,i) | i <- [min y w..max y w]]
  | otherwise = []

bigX ((x,_),(z,_)) = max x z
bigY ((_,y),(_,w)) = max y w

compute1 :: [(Point,Point)] -> Int
compute1 ls = length $ filter (1 <) $ elems arr
  where
    xmax = maximum $ map bigX ls
    ymax = maximum $ map bigY ls
    arr = accumArray (+) 0 ((0,0),(xmax,ymax)) $
          [ (p,1) | l <- ls, p <- makeLine l]

test1 = main1 "sample.txt"
run1 = main1 "input.txt"

{-
*Main> test1
5
*Main> run1
7297
-}

main2 fn = do
  co <- readFile fn
  let dat = map parseLine $ lines co
  print $ compute2 dat

test2 = main2 "sample.txt"
run2 = main2 "input.txt"

compute2 :: [(Point,Point)] -> Int
compute2 ls = length $ filter (1 <) $ elems arr
  where
    xmax = maximum $ map bigX ls
    ymax = maximum $ map bigY ls
    arr = accumArray (+) 0 ((0,0),(xmax,ymax)) $
          [ (p,1) | l <- ls, p <- makeLine4 l] -- makeLine2,3でもヨシ

makeLine2 :: (Point,Point) -> [Point]
makeLine2 pp@((x,y),(z,w)) = loop x y
  where
    dx = signum (z - x)
    dy = signum (w - y)
    loop a b
      | a == z && b == w = [(a,b)]
      | otherwise = (a,b) : loop (a+dx) (b+dy)

-------

makeLine3 :: (Point,Point) -> [Point]
makeLine3 (xy@(x,y),zw@(z,w)) = xy : unfoldr step xy
  where
    dx = signum (z - x)
    dy = signum (w - y)
    step ab@(a,b)
      | ab == zw = Nothing
      | otherwise = let ab1 = (a+dx,b+dy) in Just (ab1, ab1)

breakOn :: (a -> Bool) -> [a] -> [a]
breakOn p [] = []
breakOn p (x:xs)
  | p x = [x]
  | otherwise = x : breakOn p xs

makeLine4 :: (Point,Point) -> [Point]
makeLine4 (xy@(x,y),zw@(z,w)) = breakOn (zw ==) $ iterate step xy
  where
    dx = signum (z - x)
    dy = signum (w - y)
    step (a,b) = (a+dx,b+dy)

{-
*Main> test2
12
*Main> run2
21038
-}
