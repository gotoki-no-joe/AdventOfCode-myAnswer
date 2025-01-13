# 入力

たまには真面目に、行ごとに、コロンの前後で分けて、前半はひとつの整数、後半は整数列にして対にする。

```haskell
runner i f = do
  ls <- lines <$> readFile i
  print $ f $ map parse ls

parse :: String -> (Int, [Int])
parse l = (read as, map read $ words bs)
  where
    (as,_:bs) = break (':' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [(Int,[Int])] -> Int
part1 rxss = ...
  where
```

# パート1

右の先頭の値から始めて、それぞれのステップまでで作れる数のリストを維持して、
二つの演算の結果でリストを伸ばしていく方法でやってみる。

```haskell
    compute (x:xs) = foldl step [x] xs
    step ys x = map (x +) ys ++ map (x *) ys
```

右の結果の中に左が含まれているものを選び出し、総和を求める。

```haskell
part1 rxss = sum [ r | (r,xs) <- rxss, elem r $ compute xs]
  where
    ...
```

右の数ごとに要素は倍に増えるのでどうかと思ったが、特に時間がかかることもなかった。
このままで最適化するとすれば、左の数を超えたら途中でも捨てる、くらいか。

# パート2

演算が増えただけ。`‖` 演算は、右の桁数だけの10の階乗を左に掛けて、右を足せばいい。
`read (show l ++ show r)` でもいいけど…

```haskell
    ll x y = y * mag + x
      where
        mag = 10 ^ length (show x)
-- ズボラな版
    ll x y = read $ show y ++ show x
```

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: [(Int,[Int])] -> Int
part2 rxss = sum [ r | (r,xs) <- rxss, elem r $ compute r xs]
  where
    compute r (x:xs) = foldl (step r) [x] xs
    step r ys x = filter (r >=) $ map (x +) ys ++ map (x *) ys ++ map (ll x) ys
```

打ち切りを入れた。それでもまだ遅い。
値の重複がキャンセルされることを期待して、`IntSet` を使ってみる。

```haskell
test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a

part2a :: [(Int,[Int])] -> Int
part2a rxss = sum [ r | (r,xs) <- rxss, IS.member r $ compute r xs]
  where
    compute r (x:xs) = foldl (step r) (IS.singleton x) xs
    step r ys x = fst $ IS.split (succ r) $ IS.unions
      [IS.mapMonotonic (x +) ys, IS.mapMonotonic (x *) ys, IS.mapMonotonic (ll x) ys]
    ll x y = y * mag + x
      where
        mag = 10 ^ length (show x)
```

割と本気だったのだけど、測ってみると(ghciで)

```haskell
timeit action = do
  t1 <- getCPUTime
  action
  t2 <- getCPUTime
  print $ t2 - t1
```

| 実装 | 時間 |
|:-:|:--|
|リスト|5秒|
|IntMap|6.5秒|

リストの方が速かった。残念。
