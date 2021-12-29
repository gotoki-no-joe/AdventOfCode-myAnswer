{-
これはなんか面倒な感じだねぇ。
とりあえずparserは書ける。
その先、何をすればいいのかしら？
-}

import Text.Read
import Data.List.Split
import Control.Monad

{-
data Pos = Pos Int Int Int deriving Show

instance Read Pos where
  readPrec = do
    x <- readPrec
    c <- get
    when (c /= ',') pfail
    y <- readPrec
    c <- get
    when (c /= ',') pfail
    z <- readPrec
    return $ Pos x y z
-}
type Pos = (Int,Int,Int)

parse :: String -> [[Pos]]
parse xs = map (map (read . ('(' :) . (++ ")")) . tail) $ splitWhen null $ lines xs

{-
0番スキャナ以外に対して、24の向きのバリエーションを全て作る必要がある。
座標軸の正の方向が集まる象限がどれになるか8とおり、それぞれどちらむきか3通り、合計24通り。
問題文の言い方だと、どちらを向いているかがXYZ*+-で6とおり、どれがX軸の正方向かでそれぞれ4通り、と。

ていうか、X横軸として、Yを縦軸、Zを奥行軸とするか、Yが奥行軸、Zが縦軸とするかで、状況が捻じ曲がるよね。
全て同じに処理するならどちらでも同じになるのかな？

それぞれのスキャナ座標に対して、スキャナ0の座標系がどうなっているか、での読み替えの方法を考えてみる。
X軸がX軸正、に4種類。Y-Zの回転が4とおり。
平面の90度回転が
 0 1
-1 0
なので、
 1  0 0
 0  0 1
 0 -1 0
を0～3回掛けると得られる。

X軸がX軸負、したとき、一番単純なのは、-x, -y, z とするもの。
-1  0 0
 0 -1 0
 0  0 1

仕切り直し。
任意の軸の任意の方向がX軸に選べて、Y軸にはそれでない任意の方向が選べて、Z軸は一意に決まるから、
6x4=24とおりある。

それぞれ、X軸、Y軸、Z軸を、どの軸の±でとるのか、を決める。
-}

t00,t01,t02,t03,t04,t05,t06,t07 :: Pos -> Pos
t10,t11,t12,t13,t14,t15,t16,t17 :: Pos -> Pos
t20,t21,t22,t23,t24,t25,t26,t27 :: Pos -> Pos
t00 (x, y, z) = ( x,  y,  z)
t01 (x, y, z) = ( x, -y, -z)
t02 (x, y, z) = ( x,  z, -y)
t03 (x, y, z) = ( x, -z,  y)
t04 (x, y, z) = (-x,  y, -z)
t05 (x, y, z) = (-x, -y,  z)
t06 (x, y, z) = (-x,  z,  y)
t07 (x, y, z) = (-x, -z, -y)
t10 (x, y, z) = ( y,  x, -z)
t11 (x, y, z) = ( y, -x,  z)
t12 (x, y, z) = ( y,  z,  x)
t13 (x, y, z) = ( y, -z, -x)
t14 (x, y, z) = (-y,  z, -x)
t15 (x, y, z) = (-y, -z,  x)
t16 (x, y, z) = (-y,  x,  z)
t17 (x, y, z) = (-y, -x, -z)
t20 (x, y, z) = ( z,  x,  y)
t21 (x, y, z) = ( z, -x, -y)
t22 (x, y, z) = ( z,  y, -x)
t23 (x, y, z) = ( z, -y,  x)
t24 (x, y, z) = (-z,  x, -y)
t25 (x, y, z) = (-z, -x,  y)
t26 (x, y, z) = (-z,  y,  x)
t27 (x, y, z) = (-z, -y, -x)

-- 手で作ってしまった。二つの軸の方向から外積で作るのが正しいのかな。

{-
スキャナaと、そのほか全てのスキャナ、例えばbの座標群に関して、重なりを調べて、12個以上共通してかつ、おかしなものがない設定を報告する。
aに対してbと、bに対してaとで同じことを2度行う。
base側の任意の点と、star側の任意の点の組み合わせで行う。
点の位置のずれを求めて、その差を全てのstarの点に
-}
