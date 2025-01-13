import Data.List.Split
import Data.Char
import Data.List
import Data.Ratio

import Debug.Trace
import Data.Maybe
import Test.QuickCheck
import Control.Applicative

runner i f = do
  pvs <- map parse . lines <$> readFile i
  print $ f pvs

-- クソ、またコンマ区切りかよ！マイナスが消されちまう！

parse :: String -> [Rational]
parse l = map ((% 1) . read) $ wordsBy p l
  where
    p '-' = False
    p c = not $ isDigit c

test1 = runner "sample.txt" $ part1 (7, 27)
main1 = runner "input.txt" $ part1 (2*10^14, 4*10^14)

part1 (lb, ub) pvs = length [() | pv:qus <- tails pvs, qu <- qus, prop1 pv qu]
  where
    prop1 (p1x:p1y:_p1z:v1x:v1y:_v1z:_) a2@(p2x:p2y:_p2z:v2x:v2y:_v2z:_) =
        isJust mts && 0 < t && 0 < s && lbr <= x && x <= ubr && lbr <= y && y <= ubr
      where
        mts = solveEq [[v1x, - v2x, p2x - p1x], [v1y, - v2y, p2y - p1y]]
        Just (t:s:_) = mts
        x = p1x + t * v1x
        y = p1y + t * v1y
        lbr = lb % 1
        ubr = ub % 1

{-
solveEq2 :: [Rational] -> Maybe [Rational]
solveEq2 [a,b,c,d,e,f]
  | det == 0  = Nothing
  | a /= 0 = Just [xa, y]
  | c /= 0 = Just [xc, y]
  | otherwise = error "a = c = 0"
  where
    det = a * d - b * c
    y = (a * f - c * e) / det
    xa = (e - b * y) / a
    xc = (f - d * y) / c

propSolveEQ2 a b c d e f = isJust mxy ==> a * x + b * y == e && c * x + d * y == f
  where
    mxy = solveEq2 [a,b,c,d,e,f]
    Just (x:y:_) = mxy
-}

{-
パート1

そも、この問題が成立するには、Z軸方向に駆け上がっていく雹、vx = vy = 0, vz /= 0 というものが存在しない、
ということが前提だ。そんなものをZを無視したら線にならずに点になってしまうから。

z座標は無視して、
p1 + t v1 = X
p2 + s v2 = X
の交点が lb≦x, y≦ub に入っているか判定せよと。

p1 + t v1 = p2 + s v2

v1 t - v2 s = p2 - p1
Zがないので、これを2連立方程式
v1x t - v2x s = p2x - p1x
v1y t - v2y s = p2y - p1y
として、
それを一般に解くルーチンを外に出す

ax + by = e
cx + dy = f

cax + cby = ce
acx + ady = af
y = (ce - af) / (cb - ad) ; (cb - ad /= 0)
x = (e - by) / a ; (a /= 0)
x = (f - dy) / c ; (c /= 0)
両方0のとき、それは重なるy=Y0なので困る。

p1 + t v1 として交点を求めて、それが範囲に入っているか判定する。

パート2

実際に交差する2直線を判定しないといけない。

二次元専用で作ったらいけなかったね。
p11 x1 + p12 x2 + ... + p1n xn = a1
...
pn1 x1 + pn2 x2 + ... + pnn xn = an
という、n(n+1)の値が[[Rational]]として[[p11,...,a1],...,[pn1,...,an]]と与えられたとき、
x1,...,xnの解を求める。には、
n=1のとき、[[p11,a1]] とは p1x = a1, x = a1 / p1 が解なので [-a1 / p1] を返す。p1=0ならNothingで。
n>1のとき、先頭 p*1 が0なものは、それを落としてそのまま持っておく。
それが全てのとき、なんにせよこのx1は自由なので解が一つに定まらないのでNothingで。
一つだけのとき、それ以外を再帰させる。
もっとたくさんあるとき、先頭のものと係数をたすき掛けしてp*1が0になるようにして、結局N-1個で再帰呼び出しする。
結果がNothingならNothingで、Justなら、その結果を、一つだけ残した式に代入することでx1の値も求めて、返す。
-}

solveEq :: [[Rational]] -> Maybe [Rational]
-- solveEq args | traceShow args False = error ""
solveEq [[p11,a1]]
  | p11 == 0 = Nothing
  | otherwise = Just [a1 / p11]
solveEq psas
  | null psas1 = Nothing
  | otherwise  = (x1 :) <$> mxs
  where
    (psas1, psas2) = partition ((0 /=) . head) psas
    (p11:p1sa) : psas11 = psas1
    cross (pk1:psa) = zipWith (-) (map (p11 *) psa) (map (pk1 *) p1sa)
    mxs = solveEq $ map cross psas11 ++ map tail psas2
    x1 = (last p1sa - sum (zipWith (*) p1sa (fromJust mxs))) / p11

{-
propSolveEq a b c d e f = solveEq2 [a,b,c,d,e,f] == solveEq [[a,b,e],[c,d,f]]

またバグってる。
0 x + 1/3 y = 0
1 x + 0 y = 0
x = y = 0
-}

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 pvs =
  [(i,j) | (i,pv:qus) <- zip [1 ..] $ tails pvs, (j, qu) <- zip [succ i ..] qus, parallel pv qu]
--  where
-- まず、pvどうしで交差するものを2組見つける。
-- [ (pv, qu)
-- | pv:qus <- tails pvs
-- , qu <- take 1 [qu | qu <- qus, intersect pv qu]
-- ]

-- 直線 p + tv と q + su が交差する
-- つまり、v t - u s = q - p が解を持つか判定
crossline (px:py:pz:vx:vy:vz:_) (qx:qy:qz:ux:uy:uz:_) =
  isJust m && px + t * vx == qx + s * ux && py + t * vy == qy + s * uy && pz + t * vz == qz + s * uz
  where
    m1 = solveEq [[vx, - ux, qx - px], [vy, - uy, qy - py]]
    m2 = solveEq [[vy, - uy, qy - py], [vz, - uz, qz - pz]]
    m3 = solveEq [[vz, - uz, qz - pz], [vx, - ux, qx - px]]
    m = m1 <|> m2 <|> m3
-- 解の候補 t, s が見つかって、それが正しく解である必要がある。
    Just (t:s:_) = m

{-
今回、確かに交差する例
ghci> crossline [5%1,0%1,0%1,-1%1,0%1,0%1] [0%1,0%1,100%1,0%1,0%1,-1%1]
True
は判定できて、
ghci> test2
[]
ghci> main2
[]
ということは、こいつら一つも交わらないのな。

ねじれの位置でなく、両方を通る平面が作れるような組み合わせ二つがあれば何でもいい。
交差するなら確実だけど、交差せず、かつ、ねじれの位置でないということは、
方向ベクトルが平行ってことか！
平行でなく一つの平面にあるならどこかで交わる。交わらないということは高度差がある、ねじれの位置ということ。

速度ベクトルが同じ向きで、かつ、重なっていない、同じ直線ではない、平行していることを判定しよう。
vx/ux = vy/uy = vz/uz
後半はいいか？p + vt = q となるtがない。vt = q - p
-}

parallel (px:py:pz:vx:vy:vz:_) (qx:qy:qz:ux:uy:uz:_) =
    par1 vx vy vz ux uy uz && not (par1 vx vy vz (qx - px) (qy - py) (qz - pz))
  where
    par1 x1 y1 z1 x2 y2 z2 = x1 * y2 == y1 * x2 && y1 * z2 == z1 * y2 && z1 * x2 == x1 * z2

{-
ghci> parallel [0,0,0,1,1,1] [1,0,0,2,2,2]
True
ghci> parallel [0,0,0,1,1,1] [0,0,0,1,1,1]
False
ghci> test2
[(2,3)]
ghci> main2
[]
あれ？基本的にねじれの位置？

とすると、考えていたアプローチは全く通用しない。根本から考え直しではないですか？

不明な変数を6+Nにして、始点(px,py,pz)と速度(vx,vy,vz)、それぞれの雹とぶつかる時刻 t1～tN

(px,py,pz) + ti (vx,vy,vz) = (pix, piy, piz) + ti (vix, viy, viz)
ti (vx - vix, vy - viy, vz - viz) = (pix - px, piy - py, piz - pz)
雹1個で方程式3つできるので、足りるように思えるが、
ti という変数と vxyzという変数の積があるので、一次方程式ではない、という点に注意が必要。

----

軸ごとにダイヤグラムを引いて考えると、軸ごとでなら、どう弾丸を飛ばしても必ず当たる。
ただし、全ての軸で同時に当たらないと当たったことにはならないので、ダメってこと。

ある雹が pxi, vxi という値なとき、
弾丸が x = pxi ならば、y = pyi, z = pzi と、時刻0で撃ち抜く設定するなら当てられる。
x = pxi かつ (y,z) ≠ (pyi, pzi) のとき、
vx ≠ vxi ならば、その後はどんどんずれていってしまい、二度と会えない。なので vx = vxi と決まる。
x < pxi のとき、vx ≦ vxi だと追いつけないので、vx ＞ vxi と制約される。
x > pxi のとき、vx ≧ vxi だと追いつけないので、vx ＜ vxi と制約される。
という制約が軸ごとにN個の区切りで発生する。ぴったり、も合わせて (2N+1)^3 とおり。
N=300で217,081,801

自分より下な点の速度の最小値が上限、自分より上な点の速度の最大値が下限、となる。

-}
