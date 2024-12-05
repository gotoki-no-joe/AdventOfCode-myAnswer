import Data.List.Split
import Data.List

import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

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

{- Part1
アップデート列から任意の2項を順序を保って取り出したとき、それを(a,b)として、
規則の中に(a,b)はあってもいいけどなくてもいい。
(b,a)があったらアウト。なかったらセーフ。
-}

part1 rs uss = sum $ map getCenter $ filter (correct rs) uss

correct rs us = null [() | a:bs <- tails us, b <- bs, elem (b,a) rule]
  where
    rule = S.fromList rs

-- 下のような書き方しなくても、rsのpartial applicationでruleが計算されて保持される？
-- してくれるとうれしいけどしてくれたら凄いな？

correct1 rs = prop
  where
    rule = S.fromList rs
    prop us = null [() | a:bs <- tails us, b <- bs, elem (b,a) rule]

getCenter xs = xs !! div (length xs) 2

{- Part2
updateに現れる数字を直接もつ辺だけからなるグラフを作って、
トポロジカルソートをすればいい
-}

part2 rs uss = sum $ map (getCenter . makeCorrect rs) $ filter (not . correct rs) uss

makeCorrect rs us = map v2k1 $ topSort g
  where
    (g, v2k, _) = graphFromEdges [((),a,bs) | (a,bs) <- IM.assocs im]
    uS = IS.fromList us
    im = IM.fromListWith (++) [(a,[b]) | (a,b) <- rs, IS.member a uS, IS.member b uS]
    v2k1 v = let (_,k,_) = v2k v in k
