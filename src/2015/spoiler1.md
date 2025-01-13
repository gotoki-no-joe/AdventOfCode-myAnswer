# パート1

階数を 0 から始めて、開きカッコで増やし、閉じ括弧で減らせばよい。

```haskell
part1 :: String  -- 入力
      -> Int     -- 答え
part1 = foldl step 0

step n '(' = succ n
step n ')' = pred n
```

# パート2

パート1と同様に現在位置を追跡し、最初に \\(-1\\) になったところで止めて、
そこまでの位置の個数が答えである。

```haskell
part2 :: String -> Int
part2 = length . takeWhile (-1 <) . scanl step 0
```
