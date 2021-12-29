{-# LANGUAGE Strict #-}

import Data.List.Split
import Data.List
import Data.Either

import Debug.Trace

type Pos = (Int,Int,Int)

parse :: String -> [[Pos]]
parse xs = map (map (read . ('(' :) . (++ ")")) . tail) $ splitWhen null $ lines xs

-- 外積
vprod :: Pos -> Pos -> Pos
vprod (a1,a2,a3) (b1,b2,b3) = (a2*b3-a3*b2, a3*b1-a1*b3, a1*b2-a2*b1)

-- 内積
iprod :: Pos -> Pos -> Int
iprod (a1,a2,a3) (b1,b2,b3) = a1*b1 + a2*b2 + a3*b3

-- 単位ベクトル
vunits :: [Pos]
vunits = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)]

{-
vunitsから任意に2つ、ただし垂直になる（内積が0になる）ものを選択してX,Y軸とし、
それらの外積をZ軸とする。で24の向きが作れる。
-}

type Orient = (Pos,Pos,Pos)

orients :: [Orient]
orients = [(x,y,vprod x y) | x <- vunits, y <- vunits, iprod x y == 0]

-- orientと座標の内積をとると、向きについて読み替えた座標が得られる。

translate :: Orient -> Pos -> Pos
translate (vx,vy,vz) p = (iprod vx p, iprod vy p, iprod vz p)

{-
二つのスキャナの一致を試すには、
1つめのスキャナの座標は固定しておく。
2つめのスキャナの座標は24の向き全てを試す必要がある。
両者のビーコン座標の全ての組み合わせについて、それが対応するかを試す
とは、
ズレをsubによって求め、それを2つめのスキャナのビーコン全てに足して座標を合わせ、ビーコン本体もずらして、ってそれは差そのもの、
1つめのスキャナの検出範囲にある、2つめのスキャナのビーコン座標を抽出し、
2つめのスキャナの検出範囲にある、1つめのスキャナのビーコン座標も抽出し、
両者が一致し、かつ要素数が12あれば、対応する。
そのとき向き情報とズレ情報が成果として得られる。

「スキャナの検出範囲にある」をすると外れる、という、間違いのある問題だった。
そんなことはなかった。visibleのコードをどこか間違えていたようだ。
-}

sub :: Pos -> Pos -> Pos
sub (a1,a2,a3) (b1,b2,b3) = (a1-b1, a2-b2, a3-b3)

add :: Pos -> Pos -> Pos
add (a1,a2,a3) (b1,b2,b3) = (a1+b1, a2+b2, a3+b3)

-- check1 :: [Pos] -> [Pos] -> [Pos] -- Maybe (Orient,Pos)
check1 o0 ps qs =
  case ans of
    [] -> Nothing
    (a:_) -> Just a
  where
--    ans = nub [(o,d) | o <- orients, let qs1 = map (translate o) qs, d <- try ps qs1]
    ans = [(o,d) | o <- orients, let qs1 = map (translate o) qs, d <- try ps qs1]

    try ps qs =
      [ o1
      | p <- ps, q <- qs, let o1 = sub p q
      , let qs1 = sort $ filter (visible o0) $ map (add o1) qs
      , length qs1 >= 12
      , let ps1 = sort $ filter (visible o1) ps
      , ps1 == qs1
      ]

visible :: Pos -> Pos -> Bool
visible p q = ok dx && ok dy && ok dz
  where
    (dx,dy,dz) = sub p q
    ok d = -1000 <= d && d <= 1000

{-
複数のOrientを重ね合わせるの、どうやればいいのかしらん？
平行移動なしでOrientだけ変えたもので置き換えて、平行移動だけ重ね合わせたらいいか。
-}

test1 = do
  xs <- readFile "sample.txt"
  let pss = parse xs
--  print $ filter (((68,-1246,-43) ==) . snd) $ check1 (pss !! 0) (pss !! 1)
  print $ check1 (0,0,0) (pss !! 0) (pss !! 1)

{-
あとは、スキャナ0を基準として、位置合わせをしていって、最後にすべてのビーコンの座標を統合する、まで一気に作る必要があるのか？

スキャナのリストを3つに分ける。
- 完全に処理済み : 初期値[]
- 座標確定 : 初期値は先頭、変動なし
- 調査対象 : 初期値は先頭以外残り全て
座標確定のものの先頭s0を取り出し、処理済みに入れる。
s0に対して、調査対象を全て調べ、check1で変動が求められたものと、そうでないものに分ける。
変動が求められたものについて、変動を適用し、座標確定に追加する。
check1に失敗したものは、次の調査対象に残す。
座標確定が空になったとき、調査対象も空になっているはずで、そうしたら完成。
処理済みの座標を等しいもので統合して、個数を出す。
-}

main1 fn = do
  xs <- readFile fn
  let pss = parse xs
  print $ compute1 pss

compute1 :: [[Pos]] -> Int
compute1 (ps0:pss) = length $ nub $ concat uss
  where
    uss = loop [] [((0,0,0),ps0)] pss
compute1 _ = error ""

loop :: [[Pos]] -> [(Pos,[Pos])] -> [[Pos]] -> [[Pos]]
loop uss [] [] = uss
loop uss [] _  = error "not completed"
loop uss ((o,ps):opss) qss = trace (show (length uss, succ $ length opss, length qss)) $ loop (ps:uss) (opss ++ rs) ls
  where
    chk1 qs = case check1 o ps qs of
        Nothing -> Left qs
        Just (o,d) -> Right (d, map (add d . translate o) qs)
    (ls,rs) = partitionEithers $ map chk1 qss

{-
うーん、すごい時間かかるな。
ボトムアップに作ったから、translateを毎回掛ける形になってしまったけど、それは最初にやっておけば使いまわせる情報だったわ。

様子を見られるようにして少しあんしんだけど、遅いのは変わらない。
ビーコンごとの全ての座標25個について、それを(0,0,0)にする移動をした座標集合を作って、
総当たりで一致をみるときはこれを使えば、座標の変換は不要になるけど、うーん？
どこをいじって高速化できるのかさっぱりわからんな。

*Main> main1 "sample.txt"
(0,1,4)
(1,1,3)
(2,2,1)
(3,1,1)
(4,1,0)
79

check1が「全て調べて、合わせ方が1つだけであることを確認する」は無駄だったわ。多分。
遅いのの原因の一つはこれだと思う。見つかったときしか速くはならんけど。

*Main> main1 "input.txt"
(0,1,38)
(1,1,37)
(2,3,34)
(3,5,31)
(4,5,30)
(5,6,28)
(6,7,26)
(7,7,25)
(8,8,23)
(9,7,23)
(10,6,23)
(11,8,20)
(12,8,19)
(13,7,19)
(14,9,16)
(15,8,16)
(16,7,16)
(17,8,14)
(18,7,14)
(19,6,14)
(20,5,14)
(21,4,14)
(22,4,13)
(23,4,12)
(24,3,12)
(25,2,12)
(26,1,12)
(27,1,11)
(28,1,10)
(29,2,8)
(30,1,8)
(31,2,6)
(32,4,3)
(33,4,2)
(34,4,1)
(35,4,0)
(36,3,0)
(37,2,0)
(38,1,0)
479
-}

main2 fn = do
  xs <- readFile fn
  let pss = parse xs
  print $ compute2 pss

compute2 :: [[Pos]] -> Int
compute2 (ps0:pss) = maximum [dis o1 o2 | (o1:os1) <- tails os, o2 <- os1]
  where
    os = loop2 [] [((0,0,0),ps0)] pss
compute2 _ = error ""

loop2 :: [Pos] -> [(Pos,[Pos])] -> [[Pos]] -> [Pos]
loop2 os [] [] = os
loop2 os [] _  = error "not completed"
loop2 os ((o,ps):opss) qss = trace (show (length os, succ $ length opss, length qss)) $ loop2 (o:os) (rs ++ opss) ls
  where
    chk1 qs = case check1 o ps qs of
        Nothing -> Left qs
        Just (o,d) -> Right (d, map (add d . translate o) qs)
    (ls,rs) = partitionEithers $ map chk1 qss

dis (a1,a2,a3) (b1,b2,b3) = abs (a1-b1) + abs (a2-b2) + abs (a3-b3)

{-
*Main> main2 "input.txt"
(0,1,38)
(1,1,37)
(2,3,34)
(3,5,31)
(4,6,29)
(5,6,28)
(6,5,28)
(7,4,28)
(8,4,27)
(9,7,23)
(10,7,22)
(11,8,20)
(12,8,19)
(13,7,19)
(14,6,19)
(15,6,18)
(16,5,18)
(17,5,17)
(18,4,17)
(19,4,16)
(20,4,15)
(21,4,14)
(22,5,12)
(23,4,12)
(24,5,10)
(25,7,7)
(26,7,6)
(27,6,6)
(28,6,5)
(29,5,5)
(30,4,5)
(31,4,4)
(32,3,4)
(33,4,2)
(34,3,2)
(35,2,2)
(36,2,1)
(37,1,1)
(38,1,0)
13113
一応答えは出た。
-}
