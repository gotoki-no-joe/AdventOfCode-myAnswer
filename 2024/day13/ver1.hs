{-
Aを使うには3トークンかかる
Bを使うには1トークンかかる

A/Bを使うたびに、Xに影響するかYに影響するかが切り替わる！ じゃなくて、両方動くんだ。oops
A/Bそれぞれ、Xに影響するとき、Yに影響するときの量は設定値になっている

目標ぴったりにできるものについて、トークンコストの最小値の合計を求めたい。

XA * i + XB * j = PX
YA * m + YB * n = PY
i + j ≦ m + n ≦ i + j + 1 となればよい。

コストは 3(i + m) + (j + n)
-}
import Data.Char
import Data.List.Split

import Debug.Trace
import Data.Maybe

killNonDigit c
  | isDigit c = c
  | otherwise = ' '

runner i f = do
  games <- chunksOf 6 . map read . words . map killNonDigit <$> readFile i
  let ans = f games
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part1 games = sum $ mapMaybe compute games

xcompute :: [Int] -> Maybe Int
xcompute arg@[xa, ya, xb, yb, px, py]
  | traceShow arg False = error ""
  | null cands = Nothing
  | otherwise = Just $ minimum cands
  where
    cands =
      [ 3 * (i + m) + (j + n)
      | i <- [0 .. min 100 $ div px xa], let (j, r) = divMod (px - xa * i) xb, r == 0
      , m <- [0 .. min 100 $ div py ya], let (n, s) = divMod (py - ya * m) yb, s == 0
      , elem ((i + j) - (m + n)) [0, 1]
      ]

{-
解釈を違えていた。
-}

compute :: [Int] -> Maybe Int
compute [xa, ya, xb, yb, px, py]
  | null cands = Nothing
  | otherwise = Just $ minimum cands
  where
    cands =
      [ 3 * i + j
      | i <- [0 .. min (div px xa) (div py ya)]
      , let (j, r) = divMod (px - xa * i) xb, r == 0
      , ya * i + yb * j == py
      ]

{-
ghci> test1
480
ghci> main1
29711
-}

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

part2 games = sum $ catMaybes $ map compute2 games

compute2 :: [Int] -> Maybe Int
compute2 args@[xa, ya, xb, yb, px, py]
  | det == 0  = traceShow (args,"det=0") Nothing
  | r /= 0    = traceShow (args,r,s) Nothing
  | s /= 0    = traceShow (args,r,s) Nothing
  | otherwise = Just $ 3 * i + j
  where
    px1 = px + 10000000000000
    py1 = py + 10000000000000
    det = xb * ya - yb * xa
    (j, r) = divMod (ya * px1 - xa * py1) det
    (i, s) = divMod (px1 - xb * j) xa

{-
例えば
ghci> divMod 10000000008400 94
(106382978812,72)
全く現実的でない。

XA * i + XB * j = PX
YA * i + YB * j = PY

(XA + YA) * i + (XB + YB) * j = PX + PY は役に立たない

(XA XB)(i)_(PX)
(YA YB)(j)~(PY) 逆行列を掛けたら一発ではある


これではまるで解が一つしかないみたいではないか。
2i +  j = 4
4i + 2j = 8
みたいなのだと、(0,4),(1,2),(2,0)と3つありうる。
YA≠0なのだけど、こういうときは XBYA - YBXA = 0 になる。行列式が0かこれ。

XA * i + XB * j = PX
YA * i + YB * j = PY

YA XA * i + YA XB * j = YA PX
XA YA * i + XA YB * j = XA PY

(YAXB - XAYB) * j = YAPX - XAPY
j = (YAPX - XAPY) / (YAXB - XAYB) ぜんぜんちゃうやん。

i = (PX - XB * j) / XA

解が一意に定まるものだけで済んで助かった。
det = 0 で解が無数にあるものとか、解が負になるやつとか、そういうのがなかったから。

だから、これは偶々当たっていただけで、ちゃんとした解を求めていないな。

ちゃんとやると、
det = 0 のとき、解は無数にあるか、一つもないか。
そうでなくて、r /= 0, s /= 0 のときは、ちょうど止まることはできない。
r = s = 0 のときは、答え出してヨシ。0 ≦ i,j は確認してもいいけどね。

det = 0 のとき、YAXB - XAYB = 0
(YAXA - XAYA) * i + (YAXB - XAYB) * j = YAPX - XAPY
は0 = XAPX - XAPY となるので、XAPX = XAPY なら、解は無数にある。違うとき、矛盾して、解はない。
解が無数にあるときに警告する機能を追加しよう。
-}

test2a = runner "sample.txt" part2a
main2a = runner "input.txt"  part2a

part2a games = sum $ catMaybes $ map compute2a games

compute2a :: [Int] -> Maybe Int
compute2a args@[xa, ya, xb, yb, px, py]
  | det == 0, ted /= 0 = traceShow (args, "det=0", ted) Nothing
  | det == 0, ted == 0 = traceShow (args, "infini") Nothing
  | r /= 0 || s /= 0   = Nothing
  | i < 0 || j < 0     = traceShow (args, "negative", i, j) Nothing
  | otherwise          = Just $ 3 * i + j
  where
    px1 = px + 10000000000000
    py1 = py + 10000000000000
    det = xb * ya - yb * xa
    ted = ya * px1 - xa * py1
    (j, r) = divMod (ya * px1 - xa * py1) det
    (i, s) = divMod (px1 - xb * j) xa

{-
ghci> test1
480
ghci> main1
29711
ghci> test2a
875318608908
ghci> main2a
94955433618919

はい、解が無数にあるときにどうしたらいいかはコードにできてないけれど、
入力データにそういう場合は存在しないことも確認できました。これなら合格点。

で、無数にある場合は？
2つの式が定数倍でしかないような
2i + j  = 4
4i + 2j = 8
のようなとき。しかも右辺は巨大。
しかし考えてみると、せいぜい20000以下のpx,pyを10000000000000に足したpx1,py1が、
k * px1 = py1 という関係になるはずがない。だから気にしなくてヨシ！となるんだ。なるほど。

存在しうるような問題の場合は、
cost = 3i + j
xa * i + ya * j = px1, j = (px1 - xa * i) / ya


cost = 3i + px1 / ya - xa/ya * i = (3 - xa/ya) * i + px1 / ya
3ya >=< xa により場合分けする。
=のとき、どうやっても cost = px1/ya
>のとき、

解は無数にあるけど、あらゆる点が解なわけではない。
px + qy = r という＼向きの直線の整数解、自然数解がほしい、ということ。pとqは互いに素とする。
ある(x1,y1)が解であるとき、(x1 - qk, y1 + pk) は全て解。
なので、割り切れる解があるかどうか、1+min(p,q)回の割り算を試す必要がある。
まぁそのくらいなら、このinput.txtならできるけどね。

どうせそんな場合はないので、考察はこのくらいにしておこうかね。
-}
