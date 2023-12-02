{-
depth: 5913
target: 8,701
-}

import Data.Array

myDepth = 5913 :: Int

myTarget = (8,701) :: (Int,Int)

part1 :: Int -> (Int,Int) -> Int
part1 depth target = sum $ map (flip mod 3) $ elems erosion
  where
    bnds = ((0,0),target)
-- 地質係数
    geolo = listArray bnds $ map g $ range bnds
    g (0,0) = 0
    g xy | xy == target = 0
    g (x,0) = x * 16807
    g (0,y) = y * 48271
    g (x,y) = erosion ! (pred x, y) * erosion ! (x, pred y)
-- 浸食
    erosion = listArray bnds $ map e $ range bnds
    e xy = mod (geolo ! xy + depth) 20183

sample1 = part1 510 (10,10)

ans1 = part1 myDepth myTarget

{-
0 : rocky : gear | torch | ---
1 : wet   : gear | ----- | neither
2 : narrow: ---- | torch | neither

目標ともう少し広げた範囲について、グラフを作る。
アプローチ1
各座標は、使える道具について2倍の状態を持つ。同じ状態の隣接領域への移動は距離1、違う状態へは距離7+1とする
アプローチ2
各座標は、使用中の道具について3倍の状態を持ち、それらの間に距離7の辺をおく。
隣接領域で、どちらも有効な状態についてのみ、2本の距離1の辺をおく。

なんにせよ、自分の都合だけでなくて、隣接領域のモードも気にしないとグラフが作れないな。

((0,0),torch) を始点として、(target,torch) までの距離をダイクストラ法で数えたら終わり、なのだが、
余計に検討する範囲の広さをどこまでとればいいのやら、だ。
目標ぴっちりのマップで計測して、切り替え時間を全て移動に当てたとして、目標まで到達できる範囲を入れれば？
それをもう一度行って、伸びなければ、それ以上にはならない、を、繰り返す必要がある感じ？

-}