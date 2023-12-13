import Data.List.Split
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Control.Monad

import Debug.Trace

part1 fn = do
  lr : _ : gs <- lines <$> readFile fn
  print $ solve1 lr $ M.fromList $ map parse1 gs

parse1 :: String -> (String,(String,String))
parse1 l = (fromk 0, (fromk 7, fromk 12))
  where
    fromk k = take 3 $ drop k l

solve1 lrs m = length $ takeWhile ("ZZZ" /=) $ scanl step "AAA" $ cycle lrs
  where
    step v d = (if d == 'L' then fst else snd) (m M.! v)

{-
ghci> part1 "samp1.txt"
2
ghci> part1 "samp2.txt"
6
ghci> part1 "input.txt"
12169

まぁ普通に書いてあるようにしました、と。
-}

{-
パート2

全ての ～A から同時に出発し、同じように動き、全員が同時に ～Z に到達する時刻が知りたい、と。
当該ノードは6つずつあった。
単純にするなら、それぞれを独立してlazyに走らせて、～Zに到達するステップ数を吐き出させて、
共通して出現する値でmergeすることを全体でして、出てきた最初の値でいい。
-}

inter xxs@(x:xs) yys@(y:ys) =
  case compare x y of
    LT -> inter xs yys
    GT -> inter xxs ys
    EQ -> x : inter xs ys

inters [xs] = xs
inters xss = inters $ loop xss
  where
    loop (xs:ys:xss) = inter xs ys : loop xss
    loop xss = xss

part2 fn = do
  lr : _ : gs <- lines <$> readFile fn
  print $ solve2 lr $ M.fromList $ map parse1 gs

solve2o lrs m = head $ inters $ map run starts
  where
    step v d = (if d == 'L' then fst else snd) (m M.! v)
    starts = filter (('A' ==) . last) $ M.keys m
    goal = ('Z' ==) . last
    run st = map fst $ filter (goal . snd) $ zip [0..] $ scanl step st $ cycle lrs

{-
ghci> part2 "samp3.txt"
6
ghci> part2 "input.txt"
...

まぁやっぱそうだよね。
ただ、LRどう動くかを指定されているから、それぞれの系列がいつループするのかも調べないとわからないし、
ちまちま調べるのめんどい。

多重状態だと考えるなら、726状態の6選択、重複あるから 726^6 = 146426514344294976状態の LR 遷移というグラフを考えて、
スタート位置はただ1つ、ゴールは6選択が全て～Zなところ全て、で、指定シーケンスに従って走るか。
（これを今コードで実装したことになっている。）
そうでないなら？

lrは長さ284あるようだ。
この段階で調査が必要な問題とはなぁ。
lrの倍数でAの位置に戻ってくるまでの間で、Zの位置に到達するタイミングを数える。
それらの列の最小公倍数をとる、という感じかと思うのだ。

6つのghostそれぞれについて、284の倍数ステップでいずれかのスタート位置に戻るまでの間、
いずれかのゴールに入るステップ時刻と、戻るスタート位置を全て調べる。
別々にやるべきか。まずはスタート位置。
-}

partX fn body = do
  lr : _ : gs <- lines <$> readFile fn
  print $ body lr $ M.fromList $ map parse1 gs

getCycleLen lrs m = map run starts
  where
    len = length lrs
    step v d = (if d == 'L' then fst else snd) (m M.! v)
    starts = filter isStart $ M.keys m
    isStart = ('A' ==) . last
    run st = fst $ head $ filter (isStart . snd) $
        zip [1..] $ map last $ chunksOf len $ scanl step st $ cycle lrs

-- どうやらlenちょっきりでスタートに戻ってきたりはしないようだ。
-- つまり、全ての状態から、lenステップでの動作の間のいくつ目でゴールしていて、全て走り終えるとどの状態に到達するか、
-- という大ループの一つを分析する感じ？

{-
全てのノードからlrsを走ったときの、ゴール状態に入るターン番号と、走りきったときの終着点を調べて、
グラフをたどる代わりに、これを辿って一気に調査する感じ、でどうだろう。
-}

solve2 lrs m = loop 0 starts
  where
    len = length lrs
    step v d = (if d == 'L' then fst else snd) (m M.! v) -- 1ステップ
    m2 = M.fromList [(st, foldl step st lrs) | st <- M.keys m] -- lrs一周した到着先
    goal = ('Z' ==) . last -- ゴール判定
    goals st = IS.fromList $ map fst $ filter (goal . snd) $ zip [0..] $ scanl step st lrs -- ゴールを踏むステップの集合
    gm = M.fromList [(st, goals st) | st <- M.keys m] -- の表
    starts = filter (('A' ==) . last) $ M.keys m -- スタート地点

    loop base vs
      | IS.null gs = {- trace (show (base, vs)) $ -} loop (base + len) vs1
      | otherwise  = base + IS.findMin gs
      where
        vs1 = map (m2 M.!) vs
        gs = foldl1 IS.intersection $ map (gm M.!) vs

{-
これで、正攻法のまま、圧倒的に高速化したことになるのだけど、まだダメだなぁ？コンパイルする？

上のトレースを観察した結果、ルートは6匹で交わることなく、それぞれ違う周期で、スタートにも戻らない大きなループを構成していると判明。

学生にヒントをもらったところ、それぞれのルートには一つずつしかゴールがないらしい。
ゴーストごとに、何ステップめにZに付くかをいくつか観察して、
それを並べてCRTかな。

-}

findRuns lrs m = map run starts
  where
    step v d = (if d == 'L' then fst else snd) (m M.! v)
    starts = filter isStart $ M.keys m
    isStart = ('A' ==) . last
    isGoal = ('Z' ==) . last
    run st = (\s -> (st, s)) $
        take 5 $ filter (isGoal . snd) $
        zip [0..] $ scanl step st $ cycle lrs

{-
ghci> partX "input.txt" findRuns
[("AAA",[(12169,"ZZZ"),(24338,"ZZZ"),(36507,"ZZZ"),(48676,"ZZZ"),(60845,"ZZZ")])
,("BVA",[(17263,"MLZ"),(34526,"MLZ"),(51789,"MLZ"),(69052,"MLZ"),(86315,"MLZ")])
,("HHA",[(20093,"TNZ"),(40186,"TNZ"),(60279,"TNZ"),(80372,"TNZ"),(100465,"TNZ")])
,("HVA",[(14999,"RFZ"),(29998,"RFZ"),(44997,"RFZ"),(59996,"RFZ"),(74995,"RFZ")])
,("NPA",[(20659,"BXZ"),(41318,"BXZ"),(61977,"BXZ"),(82636,"BXZ"),(103295,"BXZ")])
,("RSA",[(16697,"KPZ"),(33394,"KPZ"),(50091,"KPZ"),(66788,"KPZ"),(83485,"KPZ")])]

これ全部最初の値の倍数だわ。なのでこれらのlcmなだけだ。CRTなぞいらんかった。

ghci> foldl lcm 1 [12169, 17263, 20093, 14999, 20659, 16697]
12030780859469
-}
