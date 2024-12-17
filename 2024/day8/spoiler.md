# 入力

アンテナの記号ごとに、その位置する座標を集めてリストにしたものがあればよさそうだ。
それほど急ぐわけでもないので、文字からリストへの `Map` で表現しよう。
（速度が気になるなら、文字を例えば "AB...Zab...z0..9" の順に背番号を振って、これを添え字として配列で扱う手もある。）

フィールドの広さも必要。

```haskell
import qualified Data.Map as M

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      m = buildMap ls
  print $ f h w m

buildMap :: [String] -> M.Map Char [(Int,Int)]
buildMap ls = M.fromListWith (++) [(c,[(i,j)]) | (i, l) <- zip [1 ..] ls, (j, c) <- zip [1 ..] l, c /= '.']

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Int -> Int -> M.Map Char [(Int,Int)] -> Int
part1 h w m = ...
```

# パート1

同じ周波数のアンテナについて、その中から2つを選ぶ全ての組み合わせに関して、
片方からもう片方を中心とする点対称の位置に、つまり両側に二つの波腹が発生する。
ただし、フィールドの範囲を外れるものは無視する。

```haskell
part1 h w m = S.size antinodes
  where
    antinodes = S.fromList
      [ anti
      | ps <- M.elems m
      , (a,b):cds <- tails ps, (c,d) <- cds
      , anti <- [(c+c-a,d+d-b), (a+a-c,b+b-d)]
      , inRange ((1,1),(h,w)) anti
      ]
```

# パート2

この説明だけでは、例えば(0,0)と(2,2)にアンテナがあったとき、
(4,4)は数えることはわかるが、(1,1)を数えるべきかがはっきりしない。
まずは、それを考慮する必要があるかどうかを確認する。

これは、アンテナの相対位置が、1より大きい公約数を持たずに互いに素であればよい。
互いに素とは、最大公約数が1であることなので、全ての組み合わせについてそれを求め、
値ごとの個数を数えてみる。

```haskell
cntGCD h w m = gcdCnt
  where
    gcdCnt = M.fromListWith (+)
      [ (gcd (abs $ a - c) (abs $ b - d), 1)
      | ps <- M.elems m
      , (a,b):cds <- tails ps, (c,d) <- cds
      ]
```

結局、そういう場合はなく、相対位置の整数倍だけ考えればよいとわかった。
もしそういう場合が含まれるのなら、最大公約数で相対ベクトルを割ってからやればいいだけなので、
実際には大して違いはない。

そして、Haskellのレンジ記法を用いると、パート1よりも無精して計算できる。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Int -> Int -> M.Map Char [(Int,Int)] -> Int
part2 h w m = S.size antinodes
  where
    antinodes = S.fromList
      [ anti
      | ps <- M.elems m
      , (a,b) <- ps, (c,d) <- ps, (a,b) /= (c,d)
      , anti <- takeWhile (inRange ((1,1),(h,w))) $ zip [a, c ..] [b, d ..]
      ]
```

むしろパート1から、この `zip` が生成するリストの `(!! 2)` として無精することに気づかなかったのが「負け」だったかも。
