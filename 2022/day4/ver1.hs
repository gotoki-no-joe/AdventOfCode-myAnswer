-- 2022-12-4
import Data.List
import Data.Char
import Data.Function

-- まずはフォーマットをちゃんと読み取る。

parse :: String -> (Int,Int,Int,Int)
parse xs = (a,b,c,d)
  where
    (a:_:b:_:c:_:d:_) = map read $ groupBy ((==) `on` isDigit) xs

-- a <= c, d <= b または c <= a, b <= d であるものの個数を知りたい

prop :: (Int,Int,Int,Int) -> Bool
prop (a,b,c,d) = a <= c && d <= b || c <= a && b <= d

main1 = readFile "input.txt" >>= print . length . filter prop . map parse . lines

-- 重なりが「ない」のは、b < c または d < a であること（a <= b, c <= d は満たされていると仮定する。）
-- その否定をとればいい。

prop2 (a,b,c,d) = not (b < c || d < a) -- = b >= c && d >= a = a <= d && b >= c

main2 = readFile "input.txt" >>= print . length . filter prop2 . map parse . lines
