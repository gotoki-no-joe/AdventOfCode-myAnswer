# 入力

入力は2つの数の対の並びになっているが、縦にグループで考えるようだ。
二つの `[Int]` にしておくといいだろう。

```haskll
import Data.List

runner i f = do
  [xs, ys] <- transpose . map (map read . words) . lines <$> readFile i
  let ans = f xs ys
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int] -> Int
part1 xs ys = ...
```

# パート1

最小値を選び、差を求め、それらを取り除き…と素朴にしなくても、もちろん、昇順に整列して要素毎に比べればよい。

```haskell
part1 xs ys = sum $ map abs $ zipWith (-) (sort xs) (sort ys)
```

# パート2

入力データをざっくり眺めると、全て5桁の数のようなので、それを覆う配列に右の数の個数を集計しておいて、左の数で読み出すことでできる。
`IntMap` を使えば、データの範囲を気にする必要がなくなる。

後で同じ数を掛けるなら、右の数について個数でなくてその数の合計、を求めておけば掛け算もいらなくなる。

`IntMap` 版

```haskell
import qualified Data.IntMap as IM

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [Int] -> [Int] -> Int
part2 xs ys = sum [IM.findWithDefault 0 x cnt | x <- xs]
  where
    cnt = IM.fromListWith (+) [(y, y) | y <- ys]
```

配列版

```haskell
import Data.Array

part2a :: [Int] -> [Int] -> Int
part2a xs ys = sum $ map (cnt !) xs
  where
    cnt = accumArray (+) 0 (0,99999) $ zip ys ys
```
