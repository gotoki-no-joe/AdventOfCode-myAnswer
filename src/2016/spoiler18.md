# 素朴な解法

罠の出現条件は、現在位置の状態はトラップで、実際には「左隣と右隣が異なるとき出現」と同値である。

床が安全であるときTrue, 罠であるときFalseで表す。
すると、現在の状態の文字列に対して、次の状態を作る関数はこうなる。

```haskell
next1 bs = zipWith (==) (True:bs) (drop 1 bs ++ [True])
```

サンプルに対して動作を確認する。

```haskell
sample1 = "..^^."
sample2 = ".^^.^.^^^^"

encode1 = map ('.' ==)
decode1 = map (\b -> if b then '.' else '^')

observe1 str n = putStr $ unlines $ map decode1 $ take n $ iterate next1 $ encode1 str
```

```
ghci> observe1 sample1 3
..^^.
.^^^^
^^..^
ghci> observe1 sample2 10
.^^.^.^^^^
^^^...^..^
^.^^.^.^^.
..^^...^^^
.^^^^.^^.^
^^..^.^^..
^^^^..^^^.
^..^^^^.^^
.^^^..^.^^
^^.^^^..^^
```

指定の行数を生成し、床の個数を数える。

```haskell
compute1 str n = sum $ map (length . filter id) (take n $ iterate next1 $ encode1 str)

test1 = compute1 sample2 10
part1 = compute1 input 40
part2 = compute1 input 400000
```

```
ghci> test1
38
(0.01 secs, 85,120 bytes)
ghci> part1
1978
(0.00 secs, 836,104 bytes)
ghci> part2
20003246
(2.53 secs, 7,721,848,008 bytes)
```

# 性能を追求

ひとつのマスを1ビットで表す。ただし入力の100マスは `Int64` に入りきらないので `Integer` を用いる。

ビットシフトにより生じる空きに1をいちいち入れるのは面倒だし、`xor` を使うので、罠をビット1で表す。

```haskell
import Data.Bits

encode2 :: String -> Integer
encode2 str = foldl setBit 0 [i | (i, '^') <- zip [0 ..] str]

decode2 :: Int -> Integer -> String
decode2 w bs = [if testBit bs i then '^' else '.' | i <- [0 .. pred w]]

next2 :: Int -> Integer -> Integer
next2 w bs = clearBit (xor (shiftL bs 1) (shiftR bs 1)) w

observe2 :: String -> Int -> IO ()
observe2 str h = putStr $ unlines $ map (decode2 w) $ take h $ iterate next2 $ encode2 str
  where
    w = length str
```

いきなり本題に入ってもいいが、一応様子を観察する。

```
ghci> observe2 sample2 10
.^^.^.^^^^
^^^...^..^
^.^^.^.^^^
..^^...^..
.^^^^.^.^.
^^..^.....
^^^^.^...^
^..^..^.^.
.^^.^^...^
^^^.^^^.^^
```

何かがおかしい。  
その答えは、元の幅を超えた位置に `shiftL` で1のビットが発生してしまい、それが結果を狂わせている。
なので、`next2` の中で、一番外にはみ出たビットを削除するコードを追加する。

```haskell
next2 :: Int -> Integer -> Integer
next2 w bs = clearBit (xor (shiftL bs 1) (shiftR bs 1)) w

observe2 :: String -> Int -> IO ()
observe2 str h = putStr $ unlines $ map (decode2 w) $ take h $ iterate (next2 w) $ encode2 str
  where
    w = length str
```

これで直った。今度はビット0の個数を数える必要があるので、`popCount`から引く。

```haskell
compute2 str steps = w * steps - traps
  where
    traps = sum $ map popCount $ take steps $ iterate (next2 w) $ encode2 str
    w = length str

test1a = compute2 sample2 10
main1a = compute2 input 40
main2a = compute2 input 400000
```

```
ghci> test1a
38
(0.01 secs, 71,600 bytes)
ghci> main1a
1978
(0.00 secs, 128,320 bytes)
ghci> main2a
20003246
(0.43 secs, 310,498,792 bytes)
```

`Integer` が `Data.Bits` の対象に入っていてくれたことに感謝。

# おまけ：キャッシュ？

罠のパターンがループに陥り、400000行を真面目に調べなくとも途中から繰り返しになった可能性を検証する。

```haskell
import qualified Data.Map as M

kaburi = cnts
  where
-- 全パターンに対して+1する寄せ集め
    im = M.fromListWith (+) [(bs, 1) | bs <- take 400000 $ iterate (next2 (length input)) $ encode2 input]
-- imの値の個数をカウント。
    cnts = M.fromListWith (+) [(k, 1) | k <- M.elems im]
```

```
ghci> kaburi
fromList [(1,400000)]
(3.34 secs, 809,627,632 bytes)
```

パターンの重複は一切無かった。
