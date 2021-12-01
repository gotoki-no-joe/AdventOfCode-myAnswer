import Data.List.Split

part1 fn = do
  co <- readFile fn
  print $ compute1 $ map read $ lines co

compute1 :: [Int] -> Int
compute1 ns = length $ filter id $ zipWith (<) ns (tail ns)

{-
*Main> part1 "sample.txt"
7
*Main> part1 "input.txt"
1665
-}

part2 fn = do
  co <- readFile fn
  print $ compute2 $ map read $ lines co

compute2 :: [Int] -> Int
compute2 ns = compute1 $ map sum $ divvy 3 1 ns

{-
*Main> part2 "sample.txt"
5
*Main> part2 "input.txt"
1702
-}

-- (x2 + x3 + x4) - (x1 + x2 + x3) = x4 - x1 なので、divvy とか不要だった。
part21 fn = do
  co <- readFile fn
  print $ compute21 $ map read $ lines co

compute21 :: [Int] -> Int
compute21 ns = length $ filter id $ zipWith (<) ns (drop 3 ns)
