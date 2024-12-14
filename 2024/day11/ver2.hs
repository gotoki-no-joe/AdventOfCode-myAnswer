import qualified Data.IntMap as IM

runner i f = readFile i >>= print . f . map read . words

main1 = runner "input.txt" part1

sample0 :: [Int]
sample0 = map read $ words "0 1 10 99 999"

part1 :: [Int] -> Int
part1 xs = length $ iterate blink xs !! 25

blink :: [Int] -> [Int]
blink = foldr step []
  where
    step 0 = (1 :)
    step x
      | even l = (read as :) . (read bs :)
      | otherwise = (x * 2024 :)
      where
        ds = show x
        l = length ds
        (as,bs) = splitAt (div l 2) ds

main2 = runner "input.txt" part2

part2 :: [Int] -> Int
part2 = sizeMap . (!! 75) . iterate blinkMap . makeMap
--part2 xs = length $ iterate blink xs !! 75

makeMap :: [Int] -> IM.IntMap Int
makeMap xs = IM.fromListWith (+) [(x,1) | x <- xs]

blinkMap :: IM.IntMap Int -> IM.IntMap Int
blinkMap xm = IM.fromListWith (+) [(j,v) | (k,v) <- IM.assocs xm, j <- single k]
  where
    single 0 = [1]
    single x
      | even l = [read a, read b]
      | otherwise = [x * 2024]
      where
        ds = show x
        l = length ds
        (a,b) = splitAt (div l 2) ds

sizeMap = sum . IM.elems
