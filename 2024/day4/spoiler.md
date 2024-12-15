# 入力

二次元配列に読み込んでおけばよいだろう。

```haskell
import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Array (Int,Int) Char -> Int
part1 arr = ...
```

# パート1

まず、配列の外まで踏み出してアクセスしてしまったときにエラーにならないようなラッパーを作っておく。

```haskell
readArr arr i
  | inRange (bounds arr) i = arr ! i
  | otherwise = '#'
```

全てのマスについて、先頭が `X` であるとき、
8方向全てについて、
1. 次に `M` があり
2. その次に `A` があり
3. その次に `S` がある

ような向きを数え上げればよい。

```haskell
part1 :: Array (Int,Int) Char -> Int
part1 arr = length
  [ ()
  | (p, 'X') <- assocs arr
  , d <- ds
  , let p1 = add p  d, readArr arr p1 == 'M'
  , let p2 = add p1 d, readArr arr p2 == 'A'
  , let p3 = add p2 d, readArr arr p3 == 'S'
  ]

ds :: [(Int, Int)]
ds = [(i,j) | i <- [-1 .. 1], j <- [-1 .. 1], (i,j) /= (0,0)]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c, b+d)
```

# パート2

`A` がある位置全てについて、（左上、右下）の文字の組と、（左下、右上）の文字の組を考える。
どちらとも `('M', 'S')` または `('S', 'M')` の組み合わせならば、それは `X-MAS` である。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Array (Int,Int) Char -> Int
part2 arr = length
  [ ()
  | (p, 'A') <- assocs arr
  , let c7 = readArr arr $ add p (-1,-1)
  , let c3 = readArr arr $ add p ( 1, 1)
  , mssm c7 c3
  , let c9 = readArr arr $ add p (-1, 1)
  , let c1 = readArr arr $ add p ( 1,-1)
  , mssm c1 c9
  ]

mssm :: Char -> Char -> Bool
mssm 'M' 'S' = True
mssm 'S' 'M' = True
mssm _ _ = False
```
