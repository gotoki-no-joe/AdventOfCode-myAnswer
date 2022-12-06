-- 2022-11-17
import Data.List

parse :: String -> (Int,Int,Int)
parse xs = (read $ ws !! 3, read $ ws !! 6, read $ ws !! 13)
  where
    ws = words xs

main1 = do
  co <- readFile "input.txt"
  print $ part1 2503 $ lines co

part1 time ls = maximum
  [ s * (q * t + min t r) | (s,t,u) <- map parse ls, let (q,r) = divMod time (t + u)]

main2 = do
  co <- readFile "input.txt"
  print $ part2 2503 $ lines co

part2 time ls =
  maximum $ foldl1 (zipWith (+)) $
  map score2max $ take time $ transpose $
  [ scanl1 (+) $ cycle $ replicate t s ++ replicate u 0
  | (s,t,u) <- map parse ls ]

score2max xs = [if m == x then 1 else 0 | x <- xs]
  where
    m = maximum xs
