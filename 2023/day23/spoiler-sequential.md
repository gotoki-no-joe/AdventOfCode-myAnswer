# 入力

上下に穴が開いているのはよくないので、2行追加して塞いだ地図の二次元配列を渡す。

```haskell
import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      wall = replicate w '#'
      fld = listArray ((0, 1),(succ h, w)) $ concat $ wall : ls ++ [wall]
  print $ f h w fld

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 h w fld = ...
```

# パート1

一見、ものすごく意地悪そうな問題に見えるが、サンプルの地図をよく見てみると実は、
一方通行のマスは分岐合流する1マスを囲うようにのみ設けられていて、
それ以外の通常の通路では分岐が発生しない、とても優しい地図であるように見える。

まずはそのことを検証する。
壁でない全てのマスについて、その上下左右のマスが壁でないマスはたかだか2マス
（1マスのとき出発地点または目標地点、2マスのときは分岐のない通路）であるか、
全てが `.` でなく斜面になっている（交差点）であるか調べる。
ついでに、交差点の個数も数える。

```haskell
neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (i,j) = [(pred i,j), (succ i,j),(i, pred j),(i, succ j)]

isSlope :: Char -> Bool
isSlope c = elem c "^v<>"

checkBranch h w fld = (cond, length ncss)
  where
    ncss =
      [ ncs
      | (p, c) <- assocs fld, c /= '#'
      , let ncs = filter ('#' /=) $ map (fld !) $ neighbors p
      , 2 < length ncs]
    cond = all (all isSlope) ncss
```

```
ghci> runner "sample.txt" checkBranch
(True,7)
ghci> runner "input.txt" checkBranch
(True,34)
```

パート1だけならDFSでマスごとに数えるアプローチで答えを出せるが、
ここではまず、地図の内容を有向グラフで表現しよう。

全ての交差点の座標を取り出し、背番号を振る。
出発地点、目標地点も仲間に加えておく。

```haskell
import qualified Data.Map as M

part1 h w fld = ...
  where
-- 交差点＝上下左右に矢印が2つ以上ある '.' マス
    crosspoints = (1, 2) : (h, pred w) :
      [ p
      | (p, '.') <- assocs fld
      , 1 < length (filter (isSlope . (fld !)) $ neighbors p)]
    cpN = length crosspoints
-- 交差点の座標から背番号 リスト順なのでスタートは1,ゴールは2
    cpm = M.fromList $ zip crosspoints [1 ..]
```

グラフのノードとなるこれらの地点を、新たな記号 `+` で地図に書き込みを加える。
また、出発地点の真下と目標地点の真上にも `v` を追加して、交差点のように見せておく。

交差点でない場所は分岐がないので、「現在位置」に加えて「一つ前の位置」も持ってそうでない方に進めば、
間違いなく未知を辿ることができる。これで、交差点から出発し、隣の交差点がどこか、
そこまでの距離がいくつかを数えることができるので、
有向グラフを表す配列を構築できる。

```haskell
-- 指定された交差点の位置から、4方向のうち移動可能な方に出発し、隣の交差点までの距離とその交差点番号を組で返す
    distF p = [dist 1 p q | (c, q) <- zip "^v<>" $ neighbors p, fld1 ! q == c]
    dist d p q
      | fld1 ! q == '+' = (cpm M.! q, d)
      | otherwise = head [dist (succ d) q r | r <- neighbors q, r /= p, fld1 ! r /= '#']
-- いつもの有向グラフ形式
    g = listArray (1, cpN) $ map distF crosspoints
```

それぞれの交差点の、出発点からの最長距離は、入り辺の先の頂点のそれに辺の長さを加えたものの最大値である。
`g` を逆向きにした配列を作り、これを頼りに集めるDPを行うことで求められる。

```haskell
part1 h w fld = distG ! 2 -- 目標地点の番号は2
  where
-- グラフは結局一方通行なので、交差点の最も遠い距離は
-- 入ってくる辺の中で最も遠いもの。なのでgの逆を作ってから、それらからの距離の最大値を求める
    revg = accumArray (flip (:)) [] (1, cpN) [(q,(p,d)) | (p,qds) <- assocs g, (q,d) <- qds]
    distG = listArray (1, cpN) $ map distGF $ elems revg
    distGF pds = maximum $ 0 : [d + distG ! p | (p,d) <- pds]
```

```
ghci> test1
94
ghci> main1
2030
```

# パート2

無向グラフになったといっても、交差点はたかだか4本しか道が繋がることはないので、
単純に深さ優先探索でやっても何とかなるのではないかと思ったりもするが、何ともならない。

余りにも場合の数が多いときの対抗策として「半分全探索」という手法がある。
中間ノード34個の順列組み合わせの総当たり $34!$ を出発地点から深さ優先探索する代わりに、
出発地点から34/2=17ステップまで進んだ結果と、
目標始点から17ステップまで進んだ結果を突き合わせて、
共通する訪問済み頂点が最終到着地点である現在位置だけであるような組み合わせについて、
その距離の和の最大値を求める。

この問題の場合、全ての頂点を通るものが最善とは限らないので、一気に片方の結果を作る代わりに、incrementalに計算する。

状態は次のとおり：
- オフェンス側、各頂点 $p$ をキーとする配列。要素は頂点集合をキーとし、距離を値とするマップ。
  - 頂点集合は、始点（`S`または`E`）から $p$ に到達するある経路において、通過した頂点の集合
  - 距離は、そのような経路の中の最大値
- オフェンス側、上の配列の、最新の要素だけ取り出した一部
- ディフェンス側、同様の配列を二つ。
- 調査するべき残りのステップ数

頂点集合はビットで表現することで、マップも `IntMap` が使えるようになる。

計算は次のとおり：
- 残りステップ数が0になったら、ループを終了する。
- オフェンス側の（前回の）最新の各要素から、さらに1ステップ進めた次の状況を作成する
（経路最大でないものを消すように注意）
- 最新情報を全体の情報に統合する
- 最新情報の要素について、ディフェンス側のマップと照合し、合流する相手があれば距離を求める
- 攻守を交代し、次のループに進む

ループの各回で得られた距離の最大値が求める答えになに。

こうすることで、交差点の個数の偶奇に影響されず、必要なステップ数で計算を終了できる。

プログラムはパート1と共通部分が多いので追加で対応する。

```haskell
import qualified Data.IntMap as IM
import Data.Bits

part12 h w fld = (distG ! 2, dists2, maximum $ map snd dists2)
  where
    ...
-- パート2

--  無向にしたグラフ
    gg = listArray (1, cpN) $ zipWith (++) (elems g) (elems revg) :: Array Int [(Int,Int)]
-- oarr (スタートから訪問した頂点集合、最長距離）
-- oagent 登録されたばかりのoarrの要素
-- darr (ゴールから訪問した頂点集合、最長距離)
-- dagent 最新の要素
    oarr0 = listArray (1,cpN) $ IM.singleton (bit 1) 0 : repeat IM.empty
    darr0 = listArray (1,cpN) $ IM.empty : IM.singleton (bit 2) 0 : repeat IM.empty
    dists2 = loop cpN oarr0 oarr0 darr0 darr0
    loop :: Int -> Array Int (IM.IntMap Int) -> Array Int (IM.IntMap Int)
                -> Array Int (IM.IntMap Int) -> Array Int (IM.IntMap Int) -> [(Int,Int)]
    loop cnt _ _ _ _ | cnt <= 0 = [] -- おしまい。
    loop cnt oarr oagents darr dagents = (cpN - cnt, maximum res) : loop (pred cnt) darr dagents oarr1 oagents1
      where
        res = 0 : [dp + dq | (p, vpdp) <- assocs oagents, (vp, dp) <- IM.assocs vpdp, (vq, dq) <- IM.assocs $ darr ! p, vp .&. vq == bit p]
        oagents1 = accumArray (IM.unionWith max) IM.empty (1,cpN)
            [ (q, IM.singleton vq dpd)
            | (p, vpdp) <- assocs oagents, (vp, dp) <- IM.assocs vpdp, (q, dd) <- gg ! p, not $ testBit vp q
            , let vq = setBit vp q, let dpd = dp + dd ]
        oarr1 = accum (IM.unionWith max) oarr $ assocs oagents1
```

走らせる。

```
ghci> test12
(94,[(0,0),(1,0),(2,0),(3,0),(4,0),(5,90),(6,94),(7,126),(8,154)],154)
ghci> main12
(2030,[(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0),(10,0)
,(11,2006),(12,2030),(13,2450),(14,2498),(15,2970),(16,2970),(17,3290)
,(18,3290),(19,3866),(20,4038),(21,4166),(22,4382),(23,4482),(24,4934)
,(25,4934),(26,5278),(27,Interrupted.
```

インタプリタだとこの辺りで辛くなるので、コンパイル実行にする。
しかしコンパイルしたコードは、遅延評価で途中まで確定している `dists2` の内容を
逐次出力してくれないので、`Debug.Trace` で実行状況をモニターできるようにする。

```haskell
import Debug.Trace
import System.CPUTime

part12 h w fld = (distG ! 2, maximum $ map snd dists2)
  where
    ...
    loop cnt _ _ _ _ | traceShow cnt False = error "" -- 実行状況モニター
    ...

main = do
  t0 <- getCPUTime
  main12
  t1 <- getCPUTime
  print $ t1 - t0
```

```
> ghc -O2 ver4
> ./ver4
36
35
34
...
3
2
1
(2030,6390)
62078125000000
```

後半は目に見えて遅くなるが、測ってみると答えが出るまでにかかったのは62秒。
ただしこれはCPUが走った時間そのもので、実時間では2分と少し待った。

どうしたら並列化できるだろうか。
攻守交代する代わりに、始点からと終点からの `oarr`, `darr` の最終版を作るところまで一気にやる部分が2並列にしかならない。
完成した二つの配列のそれぞれの要素を突き合わせて最適解を見つけるところは、要素ごとに独立しているので36並列にできるが。
