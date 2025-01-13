# 入力

ファイルに入れる代わりに文字列としてプログラムに埋め込んでしまう。

```haskell
import Data.Char

input = map digitToInt "(パズル入力の数字列)"
```

# パート1

`Data.List.group` で等しいものの並びに分割し、その個数とその数の並びにする。
個数は10以上になりうるので、その場合にはまた10進数の桁ごとに分解し直す必要があることに注意。

```haskell
step :: [Int] -> [Int]
step ds = concatMap f $ group ds
  where
    f ds1@(d:_)
      | l < 10    = [l, d]
      | otherwise = map digitToInt (show l) ++ [d]
      where
        l = length ds1
```

40回繰り返した結果の長さを求める。

```haskell
part1 = length ds40
  where
    ds40 = iterate step input !! 40
```

# パート2

Wikipediaの解説を見ると、
「1, 2, 3以外の数字は、シード番号にそのような数字または
同じ数字の3つを超えるランが含まれていない限り、シーケンスに現れない。」
とある。そんなことがあるのか。
それはともかく、特に計算量を節約する何かがあるわけでもなさそうなので、
ただ計算機をブン回す。

```haskell
part2 cs = length ds50
  where
    ds50 = iterate step input !! 50
```

# ふりかえり

長さが10を越えた場合が発生するのか、`Debug.Trace` を仕込んで確認したところ起きなかった。
1,2,3以外の数字が現れないとはそういうことなのか。

力まかせで解いてしまって、本当によかったのだろうか。
