# パート1

- 行ごとに、先頭と末尾のダブルクオートの2文字ぶん、コード表現は文字数が多い。
- エスケープシーケンスが現れるごとに、
  - `\\` は1文字余計に必要
  - `\"` も1文字余計に必要
  - `\x00` は3文字余計に必要（2文字が16進数字でない、などという場合がないことを祈る）

を数えて総計を求める。

```haskell
part1 :: [String] -> Int
part1 = sum . map f
  where
    f ('\\':'x':_:_:xs) = 3 + f xs
    f ('\\':'\\':xs) = succ $ f xs
    f ('\\':'\"':xs) = succ $ f xs
    f (_:xs) = f xs
    f "" = 2
```

# パート2

むしろこっちの方がやることは少ない。

- 行ごとに、全体を囲うダブルクオート2文字が余計に必要
- `"` はエスケープ1文字余計に必要
- `\` もエスケープ1文字余計に必要

を足し合わせるだけ。

```haskell
part2 :: [String] -> Int
part2 = sum . map f
  where
    f l = 2 + length (filter (flip elem "\"\\") l)

test2 = runner "test.txt" part2
main2 = runner "input.txt" part2
```
