import Data.List.Split
import Data.List

import qualified Data.IntMap as IM

import Data.Graph

import Debug.Trace

runner i f = do
  ss <- lines <$> readFile i
  let (as,_:bs) = break null ss
  let ans = f (map parse1 as) (map parse2 bs)
  print ans

parse1 :: String -> (Int,Int)
parse1 s = (read as, read bs)
  where
    (as,_:bs)= break ('|' ==) s

parse2 :: String -> [Int]
parse2 = map read . wordsBy (',' ==)

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part1 rs uss = sum $ map getCenter $ filter prop uss
  where
    rule = IM.fromListWith (++) [(a,[b]) | (a,b) <- rs]
    prop us = all p $ tails us
    p [] = True
    p (x:ys) = and [notElem x $ IM.findWithDefault [] y rule | y <- ys]
    getCenter xs = xs !! div (length xs) 2

{-
規則 a|b の言っているのは、bの左にaがあること、は、なくてもいいので確認できない。
aより右にある数cがどれも、cの右にaがあること、とは言ってないことを調べることはできる。
というややこしいロジック。
-}

{-
正しくない順序のものを正しい順序にして中央を答えよ。
最悪、permutationsで調べてもいいけどさ。
...
しかしupdateの長さが結構あって、そういうやり方は無理っぽい。

updateに現れる数字を直接もつ辺だけからなるグラフを作って、
トポロジカルソートをすればいいんだよな？

数字はすべて2桁っぽい。
-}

part2 rs uss = sum [getCenter $ makeCorrect us | us <- uss, not $ prop us]
  where
    rule = IM.fromListWith (++) [(a,[b]) | (a,b) <- rs]
    prop us = all p $ tails us
    p [] = True
    p (x:ys) = and [notElem x $ IM.findWithDefault [] y rule | y <- ys]
    getCenter xs = xs !! div (length xs) 2

    makeCorrect us = traceShowId $ filter (flip elem us) $ topSort $ buildG (10,99) [ab | ab@(a,b) <- rs, elem a us, elem b us]