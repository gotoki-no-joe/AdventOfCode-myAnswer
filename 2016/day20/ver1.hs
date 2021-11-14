{-
ファイルを読むのが面倒だ。
開始点で+1終了点+1で-1して、0になっている範囲が許可されている区間、というやつか。
-}

import qualified Data.IntMap as IM
import Data.List

part1 :: [[Int]] -> Int
part1 xss
  | fst (IM.findMin m) > 0 = 0
  | otherwise = fst $ head $ filter ((0 ==).snd) $ scanl1 f kds
  where
    m = IM.fromListWith (+) [ p | [x,y] <- xss, p <- [(x,1),(succ y,-1)]]
    kds = IM.assocs m
    f (_,acc) (k,d) = (k,acc+d)

test1 = part1 [[5,8],[0,2],[4,7]]

main1 f = do
  co <- readFile "input.txt"
  let xss = parse co
  print $ f xss

parse :: String -> [[Int]]
parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine xs = [read xs1, read xs2]
  where
    (xs1,_:xs2) = span ('-' /=) xs

{-
*Main> test1
3
*Main> main1 part1
19449262
-}

part2 :: Int -> [[Int]] -> Int
part2 ul xss
  | fst (IM.findMin m) > 0 = 0
  | otherwise = sum $ loop jes
  where
    m = IM.fromListWith (+) [ p | [x,y] <- xss, p <- [(x,1),(succ y,-1)]]
    kds = IM.assocs m
    f (_,acc) (k,d) = (k,acc+d)
    jes = scanl1 f kds

    loop [(a,0)] = [ul - a]
    loop [_] = []
    loop ((a,0):jes) = fst (head jes) - a : loop jes
    loop (_:jes) = loop jes

test2 = part2 10 [[5,8],[0,2],[4,7]]

run2 = main1 (part2 4294967296)

{-
*Main> test2
2
*Main> run2
119
-}
