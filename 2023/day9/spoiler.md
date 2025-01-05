# 入力

数値リストのリストにする。

```haskell
runner i f = do
  nss <- map (map read . words). lines <$> readFile i
  print $ f nss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [[Int]] -> Int
part1 nss = ...
```

# パート1

それぞれの列を指示通りに処理し、結果を集計する。

```haskell
part1 nss = sum $ map loop nss
  where
    loop ns
      | all (0 ==) ns = 0
      | otherwise = last ns + loop (zipWith (-) (tail ns) ns)
```

# パート2

やること変わらない？

```haskell
part2 nss = sum $ map loop nss
  where
    loop ns
      | all (0 ==) ns = 0
      | otherwise = head ns - loop (zipWith (-) (tail ns) ns)
```
