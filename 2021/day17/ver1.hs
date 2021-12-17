import Data.Array
import Data.List

import System.CPUTime

type Problem = (Int,Int,Int,Int)

-- sample
-- target area: x=20..30, y=-10..-5
sample :: Problem
sample = (20,30,-10,-5)

-- input
-- target area: x=150..171, y=-129..-70
input :: Problem
input = (150,171,-129,-70)

type State = ((Int,Int),(Int,Int)) -- 位置X,Y, 速度X,Y

step :: State -> State
step ((x,y),(dx,dy)) = ((x+dx, y+dy),(dx - signum dx, pred dy))

{-
ある初期値が命中するかどうかは、
Yの下限を超えて落ちるまでに、指定の枠内に入るかどうかで判定可能。
-}

strike :: Problem -> (Int,Int) -> Bool
strike (xmin,xmax,ymin,ymax) dxy = any inbox $ takeWhile ((ymin <=) . snd . fst) $ iterate step ((0,0),dxy)
  where
    inbox ((x,y),_) = xmin <= x && x <= xmax && ymin <= y && y <= ymax

{-
*Main> strike sample (7,2)
True
*Main> strike sample (6,3)
True
*Main> strike sample (9,0)
True
*Main> strike sample (17,-4)
False
-}

{-
strikeするような全てのdxy0のうち、最も高く上がる、つまりdy0の最大なものの、その最高到達高度を答えよ、が問題。
dy0を大きくしていくと、飛び越えてしまうようになる？いや、X方向はブレーキがかかるので、dx0 * (dx0 -1) / 2 までで止まる。
そのときdy0は任意に大きくすることができる。ただし、すごいスピードで的を通り過ぎるので、当たるかどうかは運しだい。

ではない。上に投げること(dy0>0)前提で、戻ってきたとき xi=0 速度は dyi = -dy0-1 なので、次のステップで xj = -dy0-1
これが目標に入る速度が最大。つまり ymin <= -dy0-1 <= ymax, dy0 <= -ymin+1 なので sampleでは(6,9)か(7,9)なんだ。

ということは、問題の側で、安全な、150 <= sum [1..dx] <= 171 となる dx の範囲は、
-}

xrange = takeWhile ((171 >=) . snd) $ dropWhile ((150 >) . snd) $ zip [1..] $ scanl1 (+) [1..]

{-
*Main> xrange
[(17,153),(18,171)]

で、そのときに使えるdy0 > 0 の初期値は、dy0 <= 129 - 1 = 128

*Main> strike input (17,128)
True
*Main> strike input (18,128)
True

そういう軌道の最高到達点は

*Main> sum [1..128]
8256

一応sampleも
*Main> sum [1..9]
45

うん、計算機は使っているけどプログラムはあまり書いてないな。
-}

{-
後半。まぁ案の定というか、「全ての可能な初期値をいえ」って、どうやって探すのかと。

結局、x,yが初期値によってどんな数列になるかはわかるので、全てのx,yについて個別には、その値をとりうる初期値を決めることができて、
その値をとる瞬間に、ちょうどその位置になるような相手の座標の初期値を全て見つけたらよい、ということか。
逆算で求める感じ。

もっと範囲を限定して、総当たりで計算できそう。
dx0は、ブレーキで止まるので、xminのsampleなら20に到達できるものが下限、それが6。
xmaxのsampleなら30は、dx0=30のとき時刻1でx=30になるので、これが上限。
上に投げたとき、前半での考察で、dy0 <= -ymin+1 が上限。
下に投げたとき、時刻1でyminに到達する dy0 = yminが下限。なんだ簡単じゃん。
-}

compute2 sample@(xmin,xmax,ymin,_) = length
    [ () | dx0 <- [dxmin..xmax], dy0 <- [ymin .. 1-ymin], strike sample (dx0,dy0)]
  where
    dxmin = fst $ head $ dropWhile ((xmin >) . snd) $ zip [1..] $ scanl1 (+) [1..]

test1 = compute2 sample

part2 = compute2 input

{-
*Main> test1
112
*Main> part2
2326
何秒かかかった。ちょっと力づくだったか。
-}

{-
この導いた範囲の dx0, dy0 で、毎回 iterate step するのはぜいたくなので、それをキャッシュするとすごく速くなるはず。
-}

compute3 sample@(xmin,xmax,ymin,ymax) = length [() | dx0 <- [dxmin..xmax], dy0 <- [ymin .. 1-ymin], strike dx0 dy0]
  where
    dxmin = fst $ head $ dropWhile ((xmin >) . snd) $ zip [1..] $ scanl1 (+) [1..]
    dxmax = xmax
    dymin = ymin
    dymax = 1-ymin
    xarr = listArray (dxmin,dxmax) $ map (\dx0 -> unfoldr xf (0,dx0)) [dxmin..dxmax]
    xf (x,dx) = Just (x, (x + dx, dx - signum dx))
    yarr = listArray (dymin,dymax) $ map (\dy0 -> unfoldr yf (0,dy0)) [dymin..dymax]
    yf (y,dy) = Just (y, (y + dy, pred dy))
    inbox (x,y) = xmin <= x && x <= xmax && ymin <= y && y <= ymax
    strike dx0 dy0 = any inbox $ takeWhile ((ymin <=) . snd) $ zip (xarr ! dx0) (yarr ! dy0)

{-
*Main> compute3 sample
112
*Main> compute3 input
2326
一応、速くなりました。
-}

main1 = do
  t0 <- getCPUTime
  let ans1 = compute2 input
  print ans1
  t1 <- getCPUTime
  print $ t1 - t0
  let ans2 = compute3 input
  print ans2
  t2 <- getCPUTime
  print $ t2 - t1

{-
*Main> main1
2326
5984375000000 : compute2だと6秒
2326
2375000000000 : compute3だと2.4秒

strikeのリメイクをするのでなく、inboxでいるような時刻の範囲だけもっておいて、重なりがあるかだけ調べればもっと早く済む感じか。
そこまでしなくてまぁいいや。
-}
