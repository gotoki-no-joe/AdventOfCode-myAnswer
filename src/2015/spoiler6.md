# 入力

このように、妙に冗長な入力データを読み取る必要があるのもAoCの特徴。
指示の種類を表す列挙型と、範囲を表す座標の対を組にした指示の型を定義する。

```haskell
data Cmd = LOff | LOn | LTgl

type Instr = (Cmd, ((Int,Int),(Int,Int)))
```

コンマで区切られた数字列を、二つの整数の対として読み込む補助関数を定義しておく。

```haskell
csv :: String -> (Int, Int)
csv xs = (read as, read bs)
  where
    (as, _:bs) = span (',' /=) xs
```

一行を読み込むには、まず `words` で分割し、2語めが何かで見分けると話が早い。
`toggle`だけ、左上座標がどこにあるかが違う。右下座標は常に末尾にある。

```haskell
parse :: String -> (Cmd, ((Int, Int), (Int, Int)))
parse xs =
  case w2 of
    "on"  -> (LOn , (csv w3, xy9))
    "off" -> (LOff, (csv w3, xy9))
    _     -> (LTgl, (csv w2, xy9))
  where
    (_:w2:w3:ws) = words xs
    xy9 = csv (last ws)
```

全体を読み込んで処理する。

```haskell
runner f = readFile i >>= print f . map parse . lines
```

# パート1

\\(1000 \times 1000\\) の配列を作り、指示通りにライトを操作して、最終結果を取り出せばよい。
ここで、`Data.Ix.range` を使うと、範囲の要素を列挙することが容易にできる。
その結果を `accumArray` で累積すれば結果が得られる。

```haskell
import Data.Ix
import Data.Array

part1 :: [String] -> Int
part1 ls = length $ filter id $ elems arr
  where
    arr = accumArray control1 False ((0,0),(999,999))
          [(xy, cmd) | l <- ls, let (cmd, bnds) = parse l, xy <- range bnds]

control1 :: Bool -> Cmd -> Bool
control1 _ LOn  = True
control1 _ LOff = False
control1 b LTgl = not b
```

# パート2

パート1では2値だったライトの状態を整数にし、指示に対する操作を定義通りに差し替えればよい。

```haskell
part2 :: [String] -> Int
part2 ls = sum $ elems arr
  where
    arr = accumArray control2 0 ((0,0),(999,999))
          [(xy, cmd) | l <- ls, let (cmd, bnds) = parse l, xy <- range bnds]

control2 :: Int -> Cmd -> Int
control2 n LOn  = succ n
control2 n LOff = max 0 (pred n)
control2 n LTgl = n + 2
```

