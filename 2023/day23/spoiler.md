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

一見ものすごく意地悪そうな問題に見えるが、サンプルの地図をよく見てみると実は、
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

グラフのノードとなるこれらの地点を、新たな記号 `+` と地図に書き込みを加える。
また、出発地点の真下と目標地点の真上にも `v` を追加して、交差点のように見せておく。

交差点でない場所は分岐がないので、「現在位置」に加えて「一つ前の位置」も持ってそうでない方に進めば、
間違いなく道を辿ることができる。これで、交差点から出発し、隣の交差点がどこか、
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
36頂点を全て辿る経路のステップ数は35なので、
出発地点から $\lfloor 35/2 \rfloor = 17$ ステップまで進んだ結果と、
目標始点から $35 - 17 = 18$ ステップまで進んだ結果を突き合わせて、
共通する訪問済み頂点が最終到着地点である現在位置だけであるような組み合わせについて、
その距離の和の最大値を求める。
この問題の場合、最善の経路が必ずしも全ての頂点を漏れなく通るとは限らないことに注意が必要である。

まず、半分の経路を作る段階について。

作る情報は、`Array Int (Map IntSet Int)` の形をしていて、
- 配列の添え字は頂点の番号 $p$
- 配列の要素であるマップのキーは、出発地点または目標地点から $p$ までのとある経路で訪問した頂点集合
- マップの値は、そのような頂点集合を通過して達成できる最長の経路の長さ
BFSのように、最前線を追跡し、一歩進むごとに配列を更新することを繰り返す。

頂点集合はビットで表現することで、マップも `IntMap` が使えるようになる。

プログラムはパート1と共通部分が多いので追加で対応する。

```haskell
type DistArray = Array Int (IM.IntMap Int)

--  無向にしたグラフ
    gg = listArray (1, cpN) $ zipWith (++) (elems g) (elems revg) :: Array Int [(Int,Int)]
-- oarr (スタートから訪問した頂点集合、最長距離）
    oarr0 = listArray (1,cpN) $ IM.singleton (bit 1) 0 : repeat IM.empty
-- darr (ゴールから訪問した頂点集合、最長距離)
    darr0 = listArray (1,cpN) $ IM.empty : IM.singleton (bit 2) 0 : repeat IM.empty

-- (集積した結果, 最前線だけの情報) を1ステップ更新する
    loop :: (DistArray, DistArray) -> (DistArray, DistArray)
    loop (arr, agents) = (arr1, agents1)
      where
        agents1 = accumArray (IM.unionWith max) IM.empty (1, cpN)
            [ (q, IM.singleton vq dpd)
            | (p, vpdp) <- assocs agents                -- pにいるエージェント
            , (vp, dp) <- IM.assocs vpdp                -- 訪問頂点vp, 総距離dp
            , (q, dd) <- gg ! p, not $ testBit vp q     -- pの隣接頂点q,距離dd、qはvpに含まれない
            , let vq = setBit vp q, let dpd = dp + dd ] -- qに進む
        arr1 = accum (IM.unionWith max) arr $ assocs agents1

    (oarrZ, _) = iterate loop (oarr0, oarr0) !! div (pred cpN) 2
    (darrZ, _) = iterate loop (darr0, darr0) !! (pred cpN - div (pred cpN) 2)
```

ここまでで様子を見てみる。

```haskell
part12 h w fld = (distG ! 2, peek)
  where
    ...
    peek = zip3 [1 ..] (map IM.size $ elems oarrZ) (map IM.size $ elems darrZ)
```

```
ghci> test12
(94,[(1,1,0),(2,0,1),(3,2,3),(4,1,2),(5,3,2),(6,2,3),(7,4,4),(8,3,2),(9,2,1)])
ghci> main12
(2030,
[(1,1,14748),(2,6748,1),(3,2433,24397),(4,1,19880),(5,8874,23227),(6,12667,21180),
(7,6710,20567),(8,5829,19949),(9,13691,21697),(10,3939,19569),(11,2433,24397)
,(12,11199,22634),(13,10789,18149),(14,12773,16830),(15,8147,14901),(16,11831,19281)
,(17,6710,20567),(18,5829,19949),(19,8413,17359),(20,10789,18149),(21,10767,12349)
,(22,8874,23227),(23,8413,17359),(24,10965,13443),(25,14909,10923),(26,12979,5899)
,(27,12667,21180),(28,10965,13443),(29,12773,16830),(30,14343,6369),(31,11199,22634)
,(32,13691,21697),(33,14748,1),(34,11831,19281),(35,12979,5899),(36,14909,10923)])
```

ノードに対して、多いところで2万通りの行き方が割り当てられた。計算時間は特に問題ない。

次に、この要素それぞれについて総当たりで、訪問頂点が現在位置以外は被らない組み合わせを見つけ、
その距離の和を求め、その最大値という最終的な答えを求める。
2万×2万で4億通り、×36頂点の計算になる。

```haskell
part12 h w fld = (distG ! 2, dists2, maximum dists2)
  where
    ...
    dists2 = zipWith dfun (elems oarrZ) (elems darrZ)

    dfun imo imd = maximum $ 0 :
       [ dp + dq
       | (vp, dp) <- IM.assocs imo  -- 出発地点からここまで来た
       , (vq, dq) <- IM.assocs imd  -- 目標地点からここまで来た
       , popCount (vp .&. vq) == 1] -- 重複は「ここ」一点のみ、という組み合わせ
```

```
ghci> test12
(94,[0,0,118,90,110,126,154,118,90],154)
ghci> main12
(2030,[3290,3290,Interrupted.
```

途端に重くなる。インタプリタでは歯が立たないのでコンパイル実行する。
しかしコンパイルしたコードは、遅延評価で途中まで確定している `dists2` の内容を
逐次出力してくれないので、`Debug.Trace` で実行状況をモニターできるようにする。

```haskell
import Debug.Trace
import System.CPUTime

part12 h w fld = (distG ! 2, maximum dists2)
  where
    ...
    dists2 = zipWith dfun (assocs oarrZ) (elems darrZ)

    dfun (v, imo) imd = traceShow (v, IM.size imo, IM.size imd) $ maximum $ 0 :
      ...

main = do
  t0 <- getCPUTime
  main12
  t1 <- getCPUTime
  print $ t1 - t0
```

```
> ghc -O2 ver5
> ./ver5
(1,1,14748)
(2,6748,1)
(3,2433,24397)
...
(35,12979,5899)
(36,14909,10923)
(2030,6390)
87203125000000
```

実時間で3分18秒、CPUTimeで87秒かかった。

## 見落としていた計算の軽量化

`dfun`の定義は問題ないように見えるが、一つ落とし穴がある。
`oarrZ` 側の`IntMap`の2万要素に対して、`darrZ` 側も毎回 `IM.assocs` を行っている。
`IM.assocs` は $O(n)$ ではあるが重いことが想像できるので、展開は一度だけにして結果を共用するようにしてみる。

```haskell
    dfun2 (v, imo) imd = traceShow (v, IM.size imo, IM.size imd) $ maximum $ 0 :
       [ dp + dq
       | (vp, dp) <- IM.assocs imo
       , (vq, dq) <- associmd           -- ココ
       , popCount (vp .&. vq) == 1]
      where
        associmd = IM.assocs imd        -- 一度だけ
```

```
> ghc -O2 ver5
> ./ver5
...
25515625000000
```

眺めているだけで目に見えて高速化している。実時間で37秒、CPUTimeで26秒。圧倒的。

## 並列化

この成果なら十分ではあるが、さらなる高速化を試みる。
これを書いている軽量ノートPCでも4コア8スレッドのcore i7が載っているので、もっと活用するべき。

`dfun2` の計算は独立しているので、これらを並列に評価させる。

```haskell
import Control.Parallel.Strategies

part12 h w fld = (distG ! 2, maximum dists3)
  where
    ...
    dists3 = runEval $ parList rpar $  -- リストの各要素を並列に計算する
             zipWith dfun2 (assocs oarrZ) (elems darrZ)
```

```
> ghc -O2 ver5 -threaded
> ./ver5 +RTS -N1
(1,1,14748)
(2,6748,1)
(7,6710,20567)
(3,2433,24397)
...
106859375000000
```

表示される`trace`の順序が入れ替わっているので、無事に並列実行されているようだ。
実時間22秒、CPUTimeで107秒…こちらが変化するのはおかしいような気がするが、よくわからない。
ともかく、実時間について37秒から15秒の短縮がこれだけ簡単な変更で達成できるのはすごい。

他の設定でも計測しておく。

| 並列度 | 実時間(秒) | CPUTime(秒) | コメント |
|:---|---:|---:|:---|
|直列版|37|26|並列化前|
|N1|55|30|オーバーヘッドのみ|
|N2|34|35|直列版に追いついただけ|
|N4|24|61|4コアを使った？|
|N|22|107|8スレッドを使った？|

前半の、`oarrZ` と `darrZ` を作る計算も独立しているので2並列にできるが、
ここはインタプリタでもひっかからない程度の時間消費だったので、効果は薄いと思われるのでそのままにしておく。

以上。
