import Data.List
import qualified Data.IntMap as IM

main1 = do
  xys <- map (map read . words) . lines <$> readFile "input.txt"
  let ans = part1 xys
  print ans

part1 :: [[Int]] -> Int
part1 xys = sum $ map abs $ zipWith (-) (sort $ map head xys) (sort $ map (!! 1) xys)

main2 = do
  xys <- map (map read . words) . lines <$> readFile "input.txt"
  let ans = part2 xys
  print ans

part2 :: [[Int]] -> Int
part2 xys = sum [x * IM.findWithDefault 0 x cnt | x:_ <- xys]
  where
    cnt = IM.fromListWith (+) [(y, 1) | _:y:_ <- xys]

{-
ghci> main1
2196996
ghci> main2
23655822
-}
