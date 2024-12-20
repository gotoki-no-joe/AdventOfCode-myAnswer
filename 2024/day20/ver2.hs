{-
チートが2ターン持続するというのは、どちらも壁でいいのか、壁は一つだけなのかよくわからない。
1つの動作に1単位時間かかる、ということは、壁は1つだけでないと埋まってしまう、ということかな。

マップの全ての位置に、開始位置からの距離と終了位置からの距離の両方を与える。
このとき、例によって壁の中まで距離を貼ってしまおう。
道な場所について、両者を足し合わせると、普通に走破したときのタイムになって、最小値なのが最短経路。

壁な場所についてもこれをやると、その壁を通り抜けたときの走破タイムになるので、
最短との差で分類できる。
-}

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

test1 = runner "sample.txt" $ part1 20 -- 変える必要なかった
main1 = runner "input.txt" $ part1 20

type POS = (Int,Int)

part1 cheatmax fld = ans2
  where
    bnds@(_,(h,w)) = bounds fld
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
-- マンハッタン距離
    mhd (a,b) (c,d) = abs (a - c) + abs (b - d)
-- もっと絞る
-- トンネルの入り口と出口をキーに、稼ぐ時間の最大値を値にMapで集計
    jm3 = M.fromListWith max
--          [(wallsof p q, gain)
          [((p1,q1), gain)
          | (p,'#') <- assocs fld
          , let ps = sDist ! index bnds p, ps /= maxBound
          , (q,'#') <- assocs fld
          , let qe = eDist ! index bnds q, qe /= maxBound
          , let m = mhd p q, m < cheatmax
          , let gain = s2e - m - ps - qe, gain > 0
          , p1 <- [p1 | p1 <- neighbors p, fld ! p1 /= '#', sDist ! index bnds p1 == pred ps]
          , q1 <- [q1 | q1 <- neighbors p, fld ! q1 /= '#', eDist ! index bnds q1 == pred qe]
          ]
    im3 = M.fromListWith max
--          [(wallsof p q, gain)
          [((p1,q1), gain)
          | (p@(x,y),'#') <- assocs fld
          , let ps = sDist ! index bnds p, ps /= maxBound
          , q <- range ((max 1 $ x - cheatmax, max 1 $ y - cheatmax), (min h $ x + cheatmax, min w $ y + cheatmax))
          , fld ! q == '#'
          , let qe = eDist ! index bnds q, qe /= maxBound
          , let m = mhd p q, m < cheatmax
          , let gain = s2e - m - ps - qe, gain > 0
          , p1 <- [p1 | p1 <- neighbors p, fld ! p1 /= '#', sDist ! index bnds p1 == pred ps]
          , q1 <- [q1 | q1 <- neighbors q, fld ! q1 /= '#', eDist ! index bnds q1 == pred qe]
          ]
-- その結果を、稼ぐ時間ごとに個数を数える
    cnt2 = IM.fromListWith (+) [(x,1) | x <- M.elems im3]
    check64 = [pq |(pq, 64) <- M.assocs im3]
-- よさそうなので、100以上稼ぐやつだけ数えて答えを出す
    ans2 = length [() | g <- M.elems im3, g >= 100]

    neighbors (i,j) = filter (inRange bnds) [(pred i,j),(succ i,j),(i,pred j),(i,succ j)]

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

で、今度は一つしかでなくなっちゃった。それは正規化しすぎ。
というのは、ゴール手前の壁をメリメリ掘って進むか、通路に出ちゃうかの選択肢があるとき、
出ちゃう方だけで正規化してしまうから。

やはり、壁の中からスタート、壁の中でゴール、とやることで、そういうのを見分ける必要がある。
スタートとゴールは完全に分かれるので、総当たりしよう。

やってみた。
[[(4,3),(5,3),(6,3),(7,3),(7,4)]
,[(4,3),(5,3),(6,3),(7,3),(7,4),(7,5)]
,[(4,3),(5,3),(6,3),(7,3),(8,3)]
,[(5,2),(6,2),(7,2),(7,3),(7,4)]
,[(5,2),(6,2),(7,2),(7,3),(7,4),(7,5)]
,[(5,2),(6,2),(7,2),(8,2),(8,3)]]
ほむ、だから、この壁の系列では、今度は穴の堀りかたが総当たりになってしまって、
始点と終点が同じなら一つとする、がなくなっているのな。

壁から壁へ、というやり方はそのままに、
その壁の外、sの距離やeの距離が1少ない近傍のペア、がキーならいいのかな。
###
##O2
  21
という場合もある？これ別に数えていい？
-}

{-
ghci> main1
827

That's not the right answer; your answer is too low.

oops.初めて誤答提出したわ。
yをxとした、しょうもないtypoを発見。

ghci> main1
1346
That's not the right answer; your answer is too low.

まだだめ？あれー。

p1とq1だけで同一視してしまうと、…いや、それでいいっていう話じゃん？

qをpとしたしょうもないtypoを発見。やいやい。

ghci> main1
767610
That's not the right answer; your answer is too low.

…まだダメってか？
-}
