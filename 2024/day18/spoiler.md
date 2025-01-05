# 入力

読み込みデータは整数のタプルのリストにする。
それとは別にグリッドの広さも与える必要がある。
さらにパート1では、データを先頭から12要素または1024要素に限定する必要もある。

```haskell
runner i f = do
  xys <- map parse . lines <$> readFile i
  print $ f xys

parse :: String -> (Int, Int)
parse l = (read as, read bs)
  where
    (as, _:bs) = break (',' ==) l

test1 = runner "samp1.txt" $ part1 6 . take 12
main1 = runner "input.txt" $ part1 70 . take 1024

part1 :: Int -> [(Int,Int)] -> Int
part1 ub xys = ...
```

# パート1

落ちてきたバイトを二次元配列に書き込んでマップを作り、
これの `(0,0)` から出発して `(ub,ub)` に到達する距離を、
1マスずつ歩いて幅優先探索で歩いて調べる計算を書くだけで特に面白みはない。

なさすぎて面倒なので、辺の長さ1に固定してダイクストラ法を呼び出して終わりにする。
無向辺で「隣接マスが `#` のときは辺を張らない」とする代わりに、
有向辺で「現在のマスが '#' のときは辺を張らない」とする方が楽なので無精する。

```haskell
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

part1 ub xys = dist
  where
    bnds = ((0,0),(ub,ub))
    fld = accumArray (&&) True bnds [(xy, False) | xy <- xys]
    l = index bnds $ fst bnds
    u = index bnds $ snd bnds

    dist = runST $ do
      arr <- dijkstra (l,u) edge (index bnds (0,0))
      readArray arr $ index bnds (ub,ub)

    i2xy = listArray (l,u) $ range bnds
    edge i
      | fld ! (x,y) = [(index bnds p, 1) | p <- [(pred x,y),(succ x,y),(x,pred y),(x,succ y)], inRange bnds p]
      | otherwise   = []
      where
        (x,y) = i2xy ! i
```

ダイクストラ法は16日目に使ったものと同じなので略。

# パート2

`input.txt` は3450行、1025行から始めて、1行ずつ追加してパート1をやりなおして、
ダメになった（距離が `maxBound :: Int` になった）ときの末尾行が答え、ではもちろんとても遅い。

1024行なら確実に通行でき、一度道が塞がれたら二度と復活することはないので、つまり二分探索を使えということ。

手持ちの、整数範囲を探索する二分探索のコードはこれ。

```haskell
binarySearch :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binarySearch prop unsat sat = loop unsat sat
  where
    loop a b
      | ende   = (a, b)
      | prop m = loop a m
      | True   = loop m b
      where
        ende = a == m || b == m
        m = div (a + b) 2
```

`prop` は探したい整数を引数にとる述語で、`sat` はこれが確かに真になる値、`unsat` はこれが偽になる値。
その間で `prop` を偽にするぎりぎりの値 `a` と真にするぎりぎりの値 `b` （つまり両者の差は1）を求める。

`sat` は12とか1024とか切り替えるのも面倒なので 0 で。
`unsat` は入力全体の長さにしておけばよいだろう。

述語 `prop k` は、長さ `k` に入力を切り飛ばして `part1` を実行したとき、
結果が `maxBound` になっていないこと、とする。

こうしたとき、リストの `b` 個目の要素が知りたいブロックである。

```haskell
test2 = runner "samp1.txt" $ part2 6
main2 = runner "input.txt" $ part2 70

part2 ub xys = xys !! pred out
  where
    prop k = part1 ub (take k xys) < maxBound
    (out, safe) = binarySearch prop (length xys) 0
```

## コンピュータの進化

昨日の17日目の問題だと探索範囲が広すぎて無茶だったけど、今日の3450とおりという桁数だと、
Ryzen Threadripper PRO 96コア192スレッドで手分けして探索したら、
パート1の計算時間の18倍程度で答えが出てしまう計算になる。

そんな時代に備えて、ヘイヘイHaskellを読み直す必要があるのかも。
ていうか、既にそういう解き方をしている人も居そうだな。

## 追記

そもそも、コンパイルして実行すれば、パート1を順に実行する力業でも大して待たずに答えが出ていた。

```haskell
import Data.Maybe

part2a ub xys = head $ dropWhile (isNothing . snd) list
  where
    prop k = part1 ub (take k xys) < maxBound
    list = [(k, if prop k then Nothing else Just (xys !! pred k)) | k <- [1 .. length xys]]
```

これにさらに、`Control.Parallel.Strategies` による並列計算を仕込んで試してみる。

```haskell
import Control.Parallel.Strategies
import System.CPUTime

part2a ub xys = head $ dropWhile (isNothing . snd) list
  where
    prop k = part1 ub (take k xys) < maxBound
    list = runEval $ parList rpar -- ココ
           [(k, if prop k then Nothing else Just (xys !! pred k)) | k <- [1 .. length xys]]

main = do
  t0 <- getCPUTime
  runner "input.txt" $ part2a 70
  t1 <- getCPUTime
  print $ t1 - t0
```

コンパイルと実行におまじないが必要。

```
> ghc -O2 ver3 -threaded -rtsopts
> ./ver3 +RTS -N1
(2906,Just (34,40))
2890625000000
> ./ver3 +RTS -N4
(2906,Just (34,40))
546875000000
> ./ver3 +RTS -N8
(2906,Just (34,40))
328125000000
```
コンパイル時に `-threaded` でマルチスレッドが使えるようになり、`-rtsopts` で `-N` オプションを渡せるようになる。
実行時に `+RTS` に続けて `-Nx` で与えた数だけコアを同時に使うようになる。
`-N1` はコア一つで実行する、いつもの状態。
このコンピュータは4コア8スレッドのi7なので、4と8を試してみた。

|コア指定|実行時間(sec)|
|:--:|---:|
| 1 | 2.89 |
| 4 | 0.55 |
| 8 | 0.33 |

ある程度計算量があり、独立性が高い部分問題が並んでいる状態だと、こんなに簡単に並列性を享受できるんだ。
（ヘイヘイHaskellでも数独ソルバの例が出ていたけれど、似たようなシチュエーションだ。）

なお、実行ファイルを起動してから、ランタイムが色々準備して実行が開始されるまでに結構時間がかかるので、
exeを起動してから0.3秒で答えが出てくる訳ではないことに注意。
もっと大きな問題にならないと体感できる程の差にはならない。

<!--
コンパイルなしでGHCiからもこれが使えたらもっと楽しいのになぁ。
-->
