import Control.Applicative

import Data.Char
import Data.List

main1 fn = do
  dss <- map (map digitToInt) . lines <$> readFile fn
  print $ compute1 dss

compute1 :: [[Int]] -> Int
compute1 dss = gamma * epsilon
  where
    len = length dss
    f ds = if sum ds * 2 > len then 1 else 0
    gamma = bin2int $ map f $ transpose dss
    epsilon = 2^(length (head dss)) - gamma - 1

bin2int :: [Int] -> Int
bin2int = loop 0
  where
    loop acc [] = acc
    loop acc (d:ds) = loop (acc * 2 + d) ds

test1 = main1 "sample.txt"
run1 = main1 "input.txt"

---- part2 ----

compute2 :: [[Int]] -> Int
compute2 dss = oxygen * co2
  where
    oxygen = bin2int $ p2proc (>=) dss
    co2 = bin2int $ p2proc (<) dss

p2proc :: (Int -> Int -> Bool) -> [[Int]] -> [Int]
p2proc _ [] = []
p2proc _ [ds] = ds
p2proc cmp dss
  | null (head dss) = []
  | otherwise = x : p2proc cmp (map tail $ filter ((x ==) . head) dss)
  where
    x = if (sum (map head dss) * 2) `cmp` length dss then 1 else 0

main2 fn = do
  dss <- map (map digitToInt) . lines <$> readFile fn
  print $ compute2 dss

test2 = main2 "sample.txt"
run2 = main2 "input.txt"

{-
*Main> test1
198
*Main> run1
2954600
*Main> test2
230
*Main> run2
1662846
-}
