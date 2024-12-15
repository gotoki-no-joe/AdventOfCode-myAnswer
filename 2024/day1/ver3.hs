import Data.List
import qualified Data.IntMap as IM
import Data.Array

runner i f = do
  [xs, ys] <- transpose . map (map read . words) . lines <$> readFile i
  let ans = f xs ys
  print ans
test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum [IM.findWithDefault 0 x cnt | x <- xs]
  where
    cnt = IM.fromListWith (+) [(y, y) | y <- ys]

test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a

part2a :: [Int] -> [Int] -> Int
part2a xs ys = sum $ map (cnt !) xs
  where
    cnt = accumArray (+) 0 (0,99999) $ zip ys ys
