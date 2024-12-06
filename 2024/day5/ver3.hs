import Data.List.Split
import Data.List

import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Graph

import Data.Array
import Data.Tuple

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

{-
まだ5日めなんだしもっと簡単に解けるはず、というか総当たりでいけるとか言っている。

まだ配置していないページの中で、他のページよりも右にないといけない、という制約がないものを一つ選び、配置する、
を繰り返せばいけそうだ。唯一に決まるのだろうし。
-}

part2a rs uss = sum $ map (getCenter . head . correctSeqs) $ filter (not . correct rs) uss
  where
    rule = S.fromList rs
    correctSeqs [] = [[]]
    correctSeqs us = [u:cs | u <- us, all (\v -> notElem (v, u) rule) us, cs <- correctSeqs $ delete u us ]

{-
part2の方が速いけど、part2aで余裕で答え出たわ。

ghci> test1
143
ghci> main1
4872
ghci> test2
123
ghci> main2
5564

「正しい並べ方が一意に存在する」という制約を悪用すると、
ruleを調べて、自分より左にあるべき、という規則を持つ、updateに含まれる数の個数、
自分がより左にあるべき、という規則を持つ、updateに含まれる数の個数、
が等しくなる数字が、探している「中心」で、他の数は眼中にない。
全ての制約が必ずあるという前提。どうだろ。
-}

part2b rs uss = sum $ map findCenter $ filter (not . correct rs) uss
  where
    toLeft  = accumArray (flip IS.insert) IS.empty (10,99) rs
    toRight = accumArray (flip IS.insert) IS.empty (10,99) $ map swap rs
    findCenter us = head [u | u <- us, cnt (toLeft ! u) == cnt (toRight ! u)]
      where
        uS = IS.fromList us
        cnt = IS.size . IS.intersection uS

{-
一番速かったwww
関係が足りないと答えを出せないプログラムだけど、
関係が足りないと、列が一意にならなくなって、答えが一意にならなくなりがちな感じ？
中央と関係ないところで自由度があるだけならいいけど、それだとこのプログラムは影響ない感じか。

まだday5だからこんなプログラムでも答えが出るような入力になっているだけか、
問題の仕様から本質的にこれで十分なのか、直観的には前者な気がそこはかとなく。
-}
