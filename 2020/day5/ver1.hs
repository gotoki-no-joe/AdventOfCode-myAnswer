import Data.List

{- ようは二進数だこれ。-}

c2b 'F' = 0
c2b 'B' = 1
c2b 'L' = 0
c2b 'R' = 1

ticket2seatid :: String -> Int
ticket2seatid xs = foldl' (\a c -> a + a + c2b c) 0 xs

test1 = map ticket2seatid ["FBFBBFFRLR","BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL"]

phase1 = do
  co <- readFile "input.txt"
  print $ maximum $ map ticket2seatid $ lines co

phase2 = do
  co <- readFile "input.txt"
  print $ compute co

compute :: String -> Int
compute co = succ $ fst $ head $ filter f $ zip ns $ tail ns
  where
    ns = sort $ map ticket2seatid $ lines co
    f (x,y) = succ x /= y

{-
*Main> test1
[357,567,119,820]
*Main> phase1
838
*Main> phase2
714
-}
