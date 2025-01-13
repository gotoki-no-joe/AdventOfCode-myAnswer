# 入力

1行め、タオルの列は個々に分解して `[String]` に、
3行め以降、デザインの列はそのままで `[String]` にしたらいい。

```haskell
runner i f = do
  l:_:ds <- lines <$> readFile i
  let ts = words $ filter (',' /=) l
  print $ f ts ds

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [String] -> [String] -> Int
part1 ts ds = ...
```

最初、1行めの列を `words` だけで分解しようとして、コンマが残ってしばらく気づかなかったのは内緒。

# パート1

それぞれのデザインについて、可能な方法を一つでいいから見つけようとする述語を通ったものを数えればいい。

```haskell
part1 ts ds = length $ filter (avail ts) ds

avail ts pat = loop pat
  where
    tls = [(t, length t) | t <- ts]
    loop "" = True
    loop xs = or [loop $ drop l xs | (t,l) <- tls, isPrefixOf t xs]
```

サンプルならこんなコードでも通るけど、本番はちょっとつらい。

上のコードの `loop` がやっている再帰計算を、[アドベントカレンダー](https://qiita.com/gotoki_no_joe/items/713af82bfd0a94e8a335)の方にも書いた `memoize` でメモ化してみる。

```haskell
    avail pat = memoize f pat
    f mf "" = ([], True)
    f mf xs = (bs, any mf bs)
      where
        bs = [b | (t,l) <- tls, let (a,b) = splitAt l xs, a == t]
```

できた。

# パート2

可能か否かの `Bool` の代わりに、場合の数を `Int` で持つように変えるだけなのでは？

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [String] -> [String] -> Int
part2 ts ds = sum $ map countWays ds
  where
    tls = [(t, length t) | t <- ts]
    countWays pat = memoize f pat
    f mf "" = ([], 1)
    f mf xs = (bs, sum $ map mf bs)
      where
        bs = [b | (t,l) <- tls, let (a,b) = splitAt l xs, a == t]
```

本当にこれだけで終わってしまった。

# 配列DP

実際にはこの問題は、上のように文字列でしなくも、
「パターンの後ろ何文字が残った状態にできる」という小さな連続範囲の整数でのDPだけでできる。

```haskell
-- array bnds nf は、添え字範囲bndsの集めるDPを行う
-- nf i は (js, fi) を返す
-- js は添え字 i の値を求めるためにDP値を集める先のリスト
-- fi は js の要素について集めてきたDP値の対のリストから、iの値を求める関数
-- 結果は、bns範囲の配列で、DPの答えが入っている
arrayDP :: Ix i => (i, i) -> (i -> ([i], [(i, b)] -> b)) -> Array i b
arrayDP bnds nf = dpArr
  where
    dpArr = listArray bnds [fi [(j, dpArr ! j) | j <- js] | i <- range bnds, let (js, fi) = nf i]

part12a :: [String] -> [String] -> (Int, Int)
part12a ts ds = (length $ filter avail ds, sum $ map countWays ds)
  where
    tls = [(t, length t) | t <- ts]
-- part1
    avail pat = arrayDP (0, len) nf ! 0
      where
        len = length pat
-- デザインの先頭 k 文字を落としたものが作れる
        nf k
          | k == len = ([], const True)
          | otherwise = (ls, or . map snd)
          where
            ls = [k + l | (t,l) <- tls, isPrefixOf t $ drop k pat]
-- part2
    countWays pat = arrayDP (0, len) nf ! 0
      where
        len = length pat
-- デザインの先頭 k 文字を落としたものを作る方法の場合の数
        nf k
          | k == len = ([], const 1)
          | otherwise = (ls, sum . map snd)
          where
            ls = [k + l | (t,l) <- tls, isPrefixOf t $ drop k pat]
```

もちろんこっちのが速い。
