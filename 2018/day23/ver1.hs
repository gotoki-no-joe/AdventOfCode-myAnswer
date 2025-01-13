import Data.List.Split
import Data.Char

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> [Int]
parse = map read . wordsBy p
  where
    p ',' = True
    p '-' = False
    p c = not $ isDigit c

test1 = runner "test1.txt" part1
main1 = runner "input.txt" part1

part1 ls = ans
  where
    rmax = maximum $ map (!! 3) ls
    maxent = head $ filter ((rmax ==) . (!! 3)) ls
    ans = length $ filter (rmax >=) $ map (mhd maxent) ls

mhd as bs = sum $ map abs $ take 3 $ zipWith (-) as bs

{-
マスに足し合わせ、は (10^9)^3 オーダーになってしまう。
どうやればいいのだこれ。

x+y+z, x-y+z, x+y-z の3つの軸に回転させた上で、座標圧縮して、
3次元の空間でいもす法を使って重なりの個数を判定する
かなぁ？
-}
