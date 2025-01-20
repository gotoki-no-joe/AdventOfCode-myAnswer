# 入力

行ごとの整数リストのリストにする。

```haskell
runner f = do
  xss <- map (map read . words) . lines <$> readFile "input.txt" :: IO [[Int]]
  print $ f xss
```

# パート1

三角形をなす条件を判定する述語を定義する。

```haskell
prop a b c = a + b > c && b + c > a && c + a > b
```

条件を満たすものを数えればよい。

```haskell
part1 = length . filter (\[a,b,c] -> prop a b c)

main1 = runner part1
```

# パート2

3行ずつに切り分けたものを転置すれば、パート1と同じ並び方になる。

```haskell
import Data.List
import Data.List.Split

part2 = part1 . concatMap transpose . chunksOf 3

main2 = runner part2
```
