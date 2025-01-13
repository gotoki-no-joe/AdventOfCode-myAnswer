import Data.List
import Data.Char
import Debug.Trace

input = "3113322113"

step :: [Int] -> [Int]
step ds = concatMap f $ group ds
  where
    f ds1@(d:_)
      | l < 10 = [l, d]
      | otherwise = traceShow l $ map digitToInt (show l) ++ [d]
      where
        l = length ds1

part1 cs = length ds40
  where
    ds0 = map digitToInt cs
    ds40 = iterate step ds0 !! 40

part2 cs = length ds50
  where
    ds0 = map digitToInt cs
    ds50 = iterate step ds0 !! 50
