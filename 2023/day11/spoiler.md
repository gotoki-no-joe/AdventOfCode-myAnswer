# 入力

行ごとの文字列のリストにしておく。

```haskell
runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = ...
  where
    ...
```

# パート1

ようは全ての点の間のマンハッタン距離を測ればよいのだが、
行および列で銀河の全くないところについては、倍を数える必要がある。
そのような行と列の位置を全て特定しておき、マンハッタン距離の計測時に考慮する。

```haskell
import Data.List

part1 ls = sum [dist p q | p:qs <- tails xys, q <- qs]
  where
-- 星の位置
    xys = [(x,y) | (x, l) <- zip [0..] ls, (y, '#') <- zip [0..] l]
-- 倍数えるべき行と列の位置
    xgaps = [x | (x, l) <- zip [0..] ls, all ('.' ==) l]
    ygaps = [y | (y, l) <- zip [0..] (transpose ls), all ('.' ==) l]
-- マンハッタン距離
    dist (x,y) (z,w) = abs (x - z) + abs (y - w) +
                       length (takeWhile (max x z >) $ dropWhile (min x z >=) xgaps) +
                       length (takeWhile (max y w >) $ dropWhile (min y w >=) ygaps)
```

# パート2

パート1の補正を+1倍でなく+999,999倍にするだけ。
修正して対応する。

```haskell
test1 = runner "sample.txt" $ part12 2
main1 = runner "input.txt"  $ part12 2
test2 = runner "sample.txt" $ part12 10
test3 = runner "sample.txt" $ part12 100
main2 = runner "input.txt"  $ part12 1000000

part12 mag ls = sum [dist p q | p:qs <- tails xys, q <- qs]
  where
-- 星の位置
    xys = [(x,y) | (x, l) <- zip [0..] ls, (y, '#') <- zip [0..] l]
-- 倍数えるべき行と列の位置
    xgaps = [x | (x, l) <- zip [0..] ls, all ('.' ==) l]
    ygaps = [y | (y, l) <- zip [0..] (transpose ls), all ('.' ==) l]
-- マンハッタン距離
    dist (x,y) (z,w) = abs (x - z) + abs (y - w) + pred mag * (gx + gy)
      where
        gx = length (takeWhile (max x z >) $ dropWhile (min x z >=) xgaps)
        gy = length (takeWhile (max y w >) $ dropWhile (min y w >=) ygaps)
```
