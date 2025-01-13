# 入力

文字 `x` で区切られているので、空白に置き換えて `words` を使えば手抜きできる。
3項組も長さ3のリストで代用する。

```haskell
runner i f = do
  lwhs <- map parse . lines <$> readFile i
  print $ f lwhs

parse :: String -> [Int,Int,Int]
parse = map read . words . map f
  where
    f 'x' = ' '
    f  c  = c
```

# パート1

要求どおりに計算するだけ。

```haskell
part1 :: [[Int]]  -- l,w,h
      -> Int      -- 答え
part1 = sum . map fun1

fun1 [l,w,h] = minimum as + 2 * sum as
  where
    as = [l * w, w * h, h * l]

main1 = runner "input.txt" part1
```

# パート2

やはり要求どおりに計算するだけ。

```haskell
part2 :: [[Int]]  -- l,w,h
      -> Int      -- 答え
part2 = sum . map fun2

fun2 [l,w,h] = 2 * minimum rs + l * w * h
  where
    rs = [l + w, w + h, h + l]
```
