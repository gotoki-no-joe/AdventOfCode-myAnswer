# 入力

入力が少量な珍しいパターン。
`[Int]` で表せばよいだろう。

```haskell
sample0 :: [Int]
sample0 = map read $ words "0 1 10 99 999"

runner i f = readFile i >>= print . f . map read . words

main1 = runner "input.txt" part1

part1 :: [Int] -> Int
part1 xs = ...
```

# パート1

一度のまばたきでの石の列の変化を計算する。

```haskell
blink :: [Int] -> [Int]
blink = foldr step []
  where
    step 0 = (1 :)
    step x
      | even l = (read as :) . (read bs :)
      | otherwise = (x * 2024 :)
      where
        ds = show x
        l = length ds
        (as,bs) = splitAt (div l 2) ds
```

```
ghci> blink sample0
[1,2024,1,0,9,9,2021976]
ghci> length (iterate blink [125, 17] !! 6)
22
ghci> length (iterate blink [125, 17] !! 25)
55312
```

同じことをすればいいので

```haskell
part1 :: [Int] -> Int
part1 xs = length $ iterate blink xs !! 25
```

できた。

# パート2

回数が上がるだけ？

```haskell
part2 :: [Int] -> Int
part2 xs = length $ iterate blink xs !! 75

main2 = runner "input.txt" part2
```

もちろんそんなに甘くない。

パート1の例で、長さ2の列が25回で55312になった。
ざっくり、1つの石が30000個に増えたと近似する。
75回は25回を3回なので、$30000 ^ 3 = 27,000,000,000,000$ 個になる。

問題文で「順序が変わらない」と妙に強調しているが、答えとして求められているのは石の個数だけである。
石の変化規則も、前後の石に影響されることなく、自分の状態だけから次が決まる。
なので、「何番の石が何個あるか」という `IntMap` で列の内容を表現することで、計算を縮小できるはず。

```haskell
part2 = sizeMap . (!! 75) . iterate blinkMap . makeMap

makeMap :: [Int] -> IM.IntMap Int
makeMap xs = IM.fromListWith (+) [(x,1) | x <- xs]

blinkMap :: IM.IntMap Int -> IM.IntMap Int
blinkMap xm = IM.fromListWith (+) [(j,v) | (k,v) <- IM.assocs xm, j <- single k]
  where
    single 0 = [1]
    single x
      | even l = [read a, read b]
      | otherwise = [x * 2024]
      where
        ds = show x
        l = length ds
        (a,b) = splitAt (div l 2) ds

sizeMap = sum . IM.elems
```

できました。
