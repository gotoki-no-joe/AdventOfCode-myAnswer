# 入力

1行の文字列を渡す。

```haskell
runner f = readFile "input.txt" >>= print . f . head . lines
```

# パート1

（スペースリークしそうな、素直なスタイルで書いておく。）

マーカーを見つけたとき、その中の掛け算の結果が伸張した結果のサイズで、
取り込む分の文字列を捨てて続きを計算する
マーカーでない文字は、そのままサイズ1と数える。

```haskell
part1 ('(':xs) = read xs1 * read xs3 + part1 xs6
  where
    (xs1, _:xs2) = break ('x' ==) xs
    (xs3, _:xs4) = break (')' ==) xs2
    xs6          = drop (read xs1) xs4
    loop (x:xs) = succ $ part1 xs
    loop [] = 0

main1 = runner part1
```

# パート2

マーカーにより複製が指示された区間について、再帰的にその一つ分の長さを数えて、繰返し回数を掛ける。

```haskell
part2 ('(':xs) = part2 xs5 * read xs3 + part2 xs6
  where
    (xs1, _:xs2) = break ('x' ==) xs
    (xs3, _:xs4) = break (')' ==) xs2
    (xs5, xs6)   = splitAt (read xs1) xs4
part2 (x:xs) = succ $ part2 xs
part2 [] = 0

main2 = runner part2
```
