-- 2025/1/14 spoiler用に書き直し

import Data.List
import Data.List.Split

runner f = do
  xss <- map (map read . words) . lines <$> readFile "input.txt" :: IO [[Int]]
  print $ f xss

prop a b c = a + b > c && b + c > a && c + a > b

part1 = length . filter (\[a,b,c] -> prop a b c)

main1 = runner part1

part2 = part1 . concatMap transpose . chunksOf 3

main2 = runner part2

{-
ghci> main1
1032
ghci> main2
1838
-}
