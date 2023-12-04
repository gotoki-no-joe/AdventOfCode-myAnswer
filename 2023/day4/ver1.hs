import Data.List
import Data.Bits

import Debug.Trace

part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve ls

solve :: [String] -> Int
solve ls = sum $ map (evalCard . words) ls
  where
    evalCard (_:_:ws) = shiftR (bit cnt) 1
      where
        (as, _:bs) = span ("|" /=) ws
        cnt = length [() | a <- as, elem a bs]

{-
ghci> part1 "sample.txt"
13
ghci> part1 "input.txt"
21138
-}

part2 fn = do
  ls <- lines <$> readFile fn
  print $ solve2 ls

solve2 ls = sum $ loop (map (const 1) ls) $ map (evalCard . words) ls
  where
    loop _ [] = []
    loop (k:ks) (c:cs) = k : loop (zipWith1 (+) ks $ replicate c k) cs

    evalCard (_:_:ws) = length [() | a <- as, elem a bs]
      where
        (as, _:bs) = span ("|" /=) ws

zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys
zipWith1 _ xs [] = xs
zipWith1 _ [] ys = ys

{-
ghci> part2 "sample.txt"
30
ghci> part2 "input.txt"
7185540
-}
