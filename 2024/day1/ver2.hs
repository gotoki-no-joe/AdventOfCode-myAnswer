import Data.List
import qualified Data.IntMap as IM

runner i f = do
  xys <- map (map read . words) . lines <$> readFile i
  let ans = f xys
  print ans

main1 = runner "input.txt" part1

main2 = runner "input.txt" part2

part1 :: [[Int]] -> Int
part1 xys = sum $ map abs $ zipWith (-) (sort $ map head xys) (sort $ map (!! 1) xys)

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
