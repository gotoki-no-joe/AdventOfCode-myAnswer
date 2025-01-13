{-
チートが2ターン持続するというのは、どちらも壁でいいのか、壁は一つだけなのかよくわからない。
1つの動作に1単位時間かかる、ということは、壁は1つだけでないと埋まってしまう、ということかな。

マップの全ての位置に、開始位置からの距離と終了位置からの距離の両方を与える。
このとき、例によって壁の中まで距離を貼ってしまおう。
道な場所について、両者を足し合わせると、普通に走破したときのタイムになって、最小値なのが最短経路。

壁な場所についてもこれをやると、その壁を通り抜けたときの走破タイムになるので、
最短との差で分類できる。
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}

import Data.Array.Unboxed

import qualified Data.Heap as H
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M

import Data.List

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls :: UArray POS Char
  print $ f fld

test1 = runner "sample.txt" $ part1 7
main1 = runner "input.txt" $ part1 21

type POS = (Int,Int)

part1 cheatmax fld = (ans, check76)
  where
    bnds = bounds fld
    spos = head [pos | (pos, 'S') <- assocs fld]
    epos = head [pos | (pos, 'E') <- assocs fld]
    arrBnds = (index bnds $ fst bnds, index bnds $ snd bnds)
    ix2pos = listArray arrBnds $ range bnds :: Array Int POS
-- 位置Sからの距離
    sDist = runSTUArray $ dijkstra arrBnds edge (index bnds spos)
    eDist = runSTUArray $ dijkstra arrBnds edge (index bnds epos)
    edge i
      | fld ! (x,y) == '#' = []
      | otherwise = [(index bnds zw, 1) | zw <- [(pred x,y),(succ x,y),(x,pred y),(x,succ y)], inRange bnds zw]
      where
        (x,y) = ix2pos ! i
-- インチキなしの距離
    s2e = sDist ! index bnds epos
-- 全ての # マスについて、s2e - sDist - eDist を求め、その値によって分類する
    im = IM.fromListWith (++)
         [(s2e - ds - de, [p]) | ((p,ds),de) <- zip (assocs sDist) (elems eDist), ds /= maxBound, de /= maxBound]
-- 問題の答え
    ans = length
          [() | ((p,ds),de) <- zip (assocs sDist) (elems eDist), ds /= maxBound, de /= maxBound, s2e - ds - de >= 100]
-- パート2
-- 壁でない全ての位置
    floors = [p | (p,c) <- assocs fld, c /= '#']
-- マンハッタン距離
    mhd (a,b) (c,d) = abs (a - c) + abs (b - d)
-- floorsから任意の2つを選択し、ただしマンハッタン距離が20以内のもの
-- 全く無駄な範囲を探さないように、fstが+20を越えたら切り捨てる効率化を入れよう、中富豪
    im2 = IM.fromListWith (+)
          [(s2e - dtotal, 1)
          | p@(i,j):qs <- tails floors
          , let ds = sDist ! index bnds p, ds /= maxBound
          , q <- takeWhile ((i + cheatmax >=) . fst) qs
          , let de = eDist ! index bnds q, de /= maxBound
          , let m = mhd p q, m <= cheatmax
          , let dtotal = ds + m + de, dtotal < s2e]
-- もっと絞る
-- かならずi座標が以下なpから、qの行まで降りる経路と、
-- そこからj座標を合わせる経路までを生成し、index整数のリストにする。
-- ただし、壁であるような座標だけ。
    wallsof (a,b) (c,d) = IS.elems $ IS.fromList $
        [index bnds pp | i <- [a .. c], let pp = (i, b), fld ! pp == '#'] ++
        [index bnds qq | j <- [min b d .. max b d], let qq = (c,j), fld ! qq == '#']
-- 穴を空ける壁のリストをキーに、稼ぐ時間の最大値を値にMapで集計
    im3 = M.fromListWith max
          [(wallsof p q, gain)
          | p@(i,j):qs <- tails floors
          , let ps = sDist ! index bnds p, ps /= maxBound, let pe = eDist ! index bnds p
          , q <- takeWhile ((i + 21 >=) . fst) qs
          , let qe = eDist ! index bnds q, qe /= maxBound, let qs = sDist ! index bnds q
          , let m = mhd p q, m <= 21
          , let gain = s2e - m - min (ps + qe) (pe + qs), gain > 0]
-- その結果を、稼ぐ時間ごとに個数を数える
    cnt2 = IM.fromListWith (+) [(x,1) | x <- M.elems im3]
    check76 = [map (ix2pos !) ps |(ps, 76) <- M.assocs im3]

-- @gotoki_no_joe
dijkstra :: (Int, Int)           -- 頂点番号の範囲
         -> (Int -> [(Int,Int)]) -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                  -- 開始点
         -> ST s (STUArray s Int Int)
dijkstra bnds edges start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    loop <- fixST $ \loop -> return $ \queue ->
      if H.null queue then return dist else do
        let Just (H.Entry cost u, queue1) = H.uncons queue
        du <- readArray dist u
        if du < cost then loop queue1 else do
          queue2 <- foldM (\q (v, we) -> do
            let duv = du + we
            dv <- readArray dist v
            if dv <= duv then return q else do
              writeArray dist v duv
              return $ H.insert (H.Entry duv v) q
            ) queue1 (edges u)
          loop queue2
    loop $ H.singleton (H.Entry 0 start)

{-
ghci> test1
fromList [(-2,60),(0,90),(2,14),(4,14),(6,2),(8,4),(10,2),(12,3),(20,1),(36,1),(38,1),(40,1),(64,1)]
得する長さとそういう位置の個数を出してみた。正しい。
-}

{-
それぞれの点について、そこからマンハッタン距離20**以下**の全ての点について、
3時以上9時未満の範囲でOK
s2e - (sDist + (マンハッタン距離) + eDist) の値でカウント

というか、任意の2点で、マンハッタン距離が20以下、という感じだね。それなら重複しない。
始点と終点を逆にした版で、短い方がショートカット。

同一視するのは1（壁）と6（床）の位置でなく、0(床)と6だったので、このやり方で大丈夫だ。
ただし距離は21までOK
それより、s.###.e と s..###..e を別に数えてしまいそう。というかする。
これを同一視するにはどうしたら？
しなくていいかもしれないから、とりあえずやってみるかぁ。

その結果：
ghci> test1
(0,fromList [(2,710),(4,758),(6,567),(8,534),(10,544),(12,562),(14,638),(16,746),(18,633),(20,769)
,(22,511),(24,529),(26,376),(28,362),(30,293),(32,290),(34,276),(36,343),(38,465),(40,342),(42,426)
,(44,427),(46,231),(48,204),(50,175),(52,194),(54,208),(56,164),(58,139),(60,133),(62,126),(64,113)
,(66,89),(68,86),(70,121),(72,90),(74,36),(76,21)])

距離は21で正しいかよくわからん。そして、明らかに数えすぎている。
そうか、全ての経路は数えなくてもいいから、決まったアルゴリズムでひとつのショートカット経路を作って、
壁を通り抜けている座標のリストか集合を作って、それをキーに、短縮距離を値にしたマップを一旦作り、
短縮距離で集計しなおせばいい。
あと、逆向きやるの忘れてた。
index化した座標の整数をソートしたものをキーにするのが高速かな？
-}

{-
だいぶ絞れたが、まだ76ステップが3つでなくて8つもあるらしい。
それを見てみるか。

[[(4,3),(5,3),(6,3),(7,3),(7,4)]
,[(4,3),(5,3),(6,3),(7,3),(7,4),(7,5)]
,[(4,3),(5,3),(6,3),(7,3),(7,4),(7,5),(7,6)]
,[(4,3),(5,3),(6,3),(7,3)                  ,(8,3)]
,[(5,2),(6,2),(7,2),(7,3),(7,4)]
,[(5,2),(6,2),(7,2),(7,3),(7,4),(7,5)]
,[(5,2),(6,2),(7,2),(7,3),(7,4),(7,5),(7,6)]
,[(5,2),(6,2),(7,2)                  ,(8,2),(8,3)]]

ごちゃごちゃしすぎたのでファイルを変える。
-}
