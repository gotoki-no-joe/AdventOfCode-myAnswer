# 入力

行ごとのリストを与える。

```haskell
runner f = readFile "input.txt" >>= print . f . lines
```

# パート1

いい文字列を判定する述語を作る。

「母音を3つ」は、`aaa` の例でもわかるように、同じ文字が3つでもよいので、数えればよい。

```haskell
isVowel x = elem x "aeiou"

cond1 xs = 3 < length (filter isVowel xs)
```

あと2つの条件のために、連続する2文字の全ての組を作っておく。

```haskell
xys = zip xs (tail xs)
```

「続き文字を含む」とは、`xys` の中に、左右が等しいものがあるということになる。

```haskell
cond2 xys = any (uncurry (==)) xys
```

特定の対を含まないとは、そのいずれも `xys` の中に現れないことである。

```haskell
cond3 xys = all (flip notElem xys) [('a','b'),('c','d'),('p','q'),('x','y')]
```

入力から、これらすべてを満たす語の数を数える。

```haskell
part1 :: [String]  -- 入力
      -> Int       -- 答え
part1 = length . filter cond123

cond123 xs = cond1 xs && cond2 xys && cond3 xys
  where
    xys = zip xs (tail xs)

main1 = runner part1
```

# パート2

前者の条件は、パート1の `xys` で考えると、直後の対は重なりがあるので、それを除いた以降に等しいものがあればよい。
全ての対について調べ、そのようなものが一つあればよい。

```haskell
cond4 xs = any sub $ tails xys
  where
    xys = zip xs (tail xs)
    sub (xy : _ : xys) = elem xy xys
    sub _ = False

-- 別解
cond4 xs = not $ null [() | xy:xys <- tails $ zip xs $ tail xs, elem xy $ drop 1 xys]
```

別解は、`drop` が不足について文句を言わないことを使っている。


後者の条件は、`xys` と同様に対になる文字について判定すればよい。
`aaa` も該当するとは、間に挟まる文字は何でもいいということなので無視できる。

```haskell
cond5 xs = any (uncurry (==)) $ zip xs (drop 2 xs)
```

以上をまとめる。

```haskell
part2 :: [String]  -- 入力
      -> Int       -- 答え
part2 = length . filter cond5 . filter cond4

main2 = runner part2
```
