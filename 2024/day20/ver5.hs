{-
ゴチャったので整理して書き直しましょう。
-}
import qualified Data.Heap as H
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

import Data.Array.Unboxed
import qualified Data.IntMap as IM

-- マンハッタン距離
mhd :: (Int, Int) -> (Int, Int) -> Int
mhd (a,b) (c,d) = abs (a - c) + abs (b - d)

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls :: UArray (Int,Int) Char
  print $ f fld

test1 = runner "sample.txt" part12
main1 = runner "input.txt" part12

part12 fld = ans2
  where
    bnds@(_,(h,w)) = bounds fld
    bfld = amap ('#' /=) fld
-- S,Eの位置
    sPos = head [pos | (pos, 'S') <- assocs fld]
    ePos = head [pos | (pos, 'E') <- assocs fld]
-- S,Eから各マスまでの距離の表、壁にもめり込む
    sDist = runSTUArray $ mazeDistance bnds (bfld !) sPos
    eDist = runSTUArray $ mazeDistance bnds (bfld !) ePos
-- チートしないときの距離
    s2e = sDist ! ePos

{-
とりあえずここまでの動作確認 s2e を返す
ghci> test1
84
ghci> main1
9324
-}

-- 全てのコース上のマスpについて、マンハッタン距離2のコース上のマスqについて、
-- PからQへ真っ直ぐ抜けたタイムは sDist ! p + 2 + eDist ! q で、s2e からひいた値が gain
-- gain > 0 の値ごとに個数を数える
    part1cnt = IM.fromListWith (+)
        [ (gain, 1)
        | (p@(x,y), True) <- assocs bfld
        , q <- [(x-2,y),(x-1,y-1),(x,y-2),(x+1,y-1),(x+2,y),(x+1,y+1),(x,y+2),(x-1,y+1)]
        , inRange bnds q, bfld ! q
        , let gain = s2e - (sDist ! p + 2 + eDist ! q)
        , gain > 0]

{-
part1cnt で、詳細な動作チェック、解釈チェックができる。

ghci> test1
fromList [(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),(20,1),(36,1),(38,1),(40,1),(64,1)]
-}

-- 本番データに関して、100以上のgainの個数を数えたらパート1の答え
    ans1 = length
        [ ()
        | (p@(x,y), True) <- assocs bfld
        , q <- [(x-2,y),(x-1,y-1),(x,y-2),(x+1,y-1),(x+2,y),(x+1,y+1),(x,y+2),(x-1,y+1)]
        , inRange bnds q, bfld ! q
        , let gain = s2e - (sDist ! p + 2 + eDist ! q)
        , gain >= 100]

{-
これでパート1は達成。

ghci> main1
1346
-}

{-
一番単純な解釈で、全ての壁でない2点P,Qの間で、マンハッタン距離20以内ので、
Pでチート発動させてQまで壁を無視して直行したときのタイムは
SからPまでの距離+PからQのマンハッタン距離+EからQのマンハッタン距離
で、これをs2eから引いたらゲインが出る。

Pを中心にダイヤ形のQの存在範囲を的確に作り出す生成器を書いてもいいけど、どうしようかな。
-}

-- (x,y)からマンハッタン距離d以内で範囲内の座標を生成
    diamond d (x,y) =
        [ (p,q)
        | p <- [max 1 $ x - d .. min h $ x + d], let e = d - abs (x - p)
        , q <- [max 1 $ y - e .. min w $ y + e] ]

{- 案1
全ての非 # マスPについて、
そこからマンハッタン距離20以内の全ての非 # マスQについて、
gain を求め、その値ごとに個数を数える
-}
    part2cnt = IM.fromListWith (+)
        [ (gain, 1)
        | (p, True) <- assocs bfld
        , q <- diamond 20 p, bfld ! q
        , let gain = s2e - (sDist ! p + mhd p q + eDist ! q)
        , gain > 0]

{-
ghci> test1
fromList [(2,138),(4,329),(6,122),(8,224),(10,109),(12,252),(14,101),(16,263),(18,94),(20,217)
,(22,76),(24,129),(26,66),(28,80),(30,61),(32,61),(34,58),(36,57),(38,51),(40,93),(42,41),(44,99)
,(46,38),(48,37),(50,32),(52,31),(54,29),(56,39),(58,25),(60,23),(62,20),(64,19),(66,12),(68,14)
,(70,12),(72,22),(74,4),(76,3)]

あってる。こんだけのことだったのか？
-}
    ans2 = length
        [ ()
        | (p, True) <- assocs bfld
        , q <- diamond 20 p, bfld ! q
        , let gain = s2e - (sDist ! p + mhd p q + eDist ! q)
        , gain >=100]

{-
ghci> main1
985482
だったようです。問題文が悪い気がする。英語読めてないけど。

ある意味、読み飛ばしていたのが悪いようです。ぐぬぬ。
-}

{-
二次元配列で表されて4近傍に移動できる迷路について、
出発地点と、座標ごとに移動の可否を答える関数を渡すと、
出発地点からの距離を全て埋めた配列を返す、距離計測プログラム

とするか、座標ごとに移動できる周囲のマスと距離を返す関数、
とするとダイクストラ法と変わらなくなる、というかPQが必要になる。
一方通行の床とかにも対応できるかと思ったのだが、そこまでするならなぁ、
という感じで、４近傍に限定した方が使い勝手がいいかも。当面は。
-}

mazeDistance :: ((Int,Int),(Int,Int)) -> ((Int,Int) -> Bool) -> (Int,Int) -> ST s (STUArray s (Int,Int) Int)
mazeDistance bnds maze start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    loop <- fixST $ \loop -> return $ \cnt ents new -> do
      case ents of
        [] | null new  -> return ()
           | otherwise -> loop (succ cnt) new []
        ((x,y):rest) -> do
          new1 <- foldM (\new q -> do
            e <- readArray dist q
            if e < maxBound then return new else do
              writeArray dist q cnt
              return (q : new)
            ) new [q | q <- [(pred x, y), (succ x, y), (x, pred y), (x, succ y)], inRange bnds q, maze q]
          loop cnt rest new1
    loop 1 [start] []
    return dist
