# 素直な実装

ドラゴンカーブは再帰呼び出しの題材としてもよく使われる。
再帰が一段深くなるたびに、普通に生成した前半を、`reverse` と要素の逆転をして後半に接続する。

この問題では、再帰の深さではなく、結果として生成された列が目標の長さに達することが目標なので、
反復ループになる。

数字の0と1をそのまま整数の0と1にして実装する。
遅延評価に依存することで、長さで `take` することを極力避けた。

```haskell
import Data.Char

-- infrate sz xs : xs を元に長さsz以上のドラゴン曲線を作る
infrate :: Int -> [Int] -> [Int]
infrate sz xs0 = loop (length xs0) xs0
  where
    loop len xs
      | sz <= len = xs
      | otherwise = loop (len + succ len) (xs ++ 0 : map (1 -) (reverse xs))

-- defrate sz xs : xsの前からszをチェックサムにする
defrate :: Int -> [Int] -> [Int]
defrate sz xs
  | even sz   = defrate (div sz 2) (checksum xs)
  | otherwise = take sz xs
  where
    checksum (a:b:as) = (if a == b then 1 else 0) : checksum as
    checksum _ = []

solve1 :: String -> Int -> String
solve1 cs sz = map intToDigit . defrate sz . infrate sz . map digitToInt $ cs
```

実行結果：

```
ghci> main1
"10011010010010010"
(0.04 secs, 232,176 bytes)
ghci> main2
"10101011110100011"
(63.05 secs, 17,565,860,336 bytes)
```

# 複数段のチェックサム計算を合成する

チェックサム計算を \\(k\\) 回適用すると元の \\(2^k\\) ビットが1ビットに縮小される。
\\(k\\) の大きい値について事前に計算するほど効率化できるが、
事前計算するべき場合の数は \\(2^{2^k}\\) になるので無茶なことはできない。
例えばきりのいい \\(k=4\\) では、65536 通りのビット列に対する表が必要になる。
\\(k=3\\) はパート2の21回と相性がいい。
\\(k=2\\) は小さすぎて、オーバーヘッドに埋もれるだろう。
\\(k \geq 5\\) は表が大きくなりすぎて無理だろう。

```haskell
import Data.Array.Unboxed
import Data.Bits

defrate2 :: Int -> [Int] -> [Int]
defrate2 sz xs
  | r == 0    = defrate2 q          (shortcut xs) -- ココ
  | even sz   = defrate2 (div sz 2) (checksum xs)
  | otherwise = take sz xs
  where
    checksum = ...
    (q,r) = divMod sz 16
    table = listArray (0, 65535)
      [ head $ checksum $ checksum $ checksum $ checksum bs    -- k回実行
      | bs <- sequence $ replicate 16 [0,1]] :: UArray Int Int
    shortcut [] = []
    shortcut xs = table ! sum (zipWith shiftL xs [0 .. 15]) : shortcut (drop 16 xs)
```

# 列の生成を仮想化する

`infrate` において、リストのコピーを実際に作っている箇所が重いと想像される。
反復的なドラゴン曲線の構築により、どのような列になっているはずかを
コンパクトな表現で表す代数的データ型を定義してみる。

```haskell
data Dragon
  = Str                      -- 与えられた数字列による列
  | Triple Dragon Int Dragon -- 前半の反転を後ろに繋いで倍にした列
  | Rev Dragon               -- 逆順にし、01を反転させた列
```

現在の長さを追跡することで、目標以上の長さを持つはずの構造を構築する。

```haskell
infrate2 :: Int -> [Int] -> [Int]
infrate2 sz xs = ...
  where
    dragon = loop (length xs) Str
    loop len d
      | sz <= len = d
      | otherwise = loop (len + succ len) (Triple d 0 (Rev d))
```

これを「前から」読んで、01列のストリームを出力する。

```haskell
infrate2 sz xs = get dragon []
  where
    dragon = ...
    loop = ...
    sx = map (1 -) (reverse xs)
    get Str                  rest = xs ++ rest
    get (Triple a b c)       rest = get a $ b : get c rest
    get (Rev (Rev d))        rest = get d rest
    get (Rev Str)            rest = sx ++ rest
    get (Rev (Triple a b c)) rest = get (Rev c) $ (1 - b) : get (Rev a) rest
```

`Rev` の適用を `get` の実行ぎりぎりまで遅延させるところがポイント。

`(1 -)` を `xor 1` にしたり、
`(if a == b then 1 else 0)` を `(a .^. b .^. 1)` や `(1 - abs (a - b))` にしたりしても
むしろ遅くなるとか、不思議。

どうせ不要だからと `checksum _ = []` の行を消すそれだけで遅くなったり。

実行結果：

```
ghci> main1a
"10011010010010010"
(1.04 secs, 351,996,872 bytes)
ghci> main2a
"10101011110100011"
(20.34 secs, 14,118,332,360 bytes)
```

大幅に改善された。

# チェックサムの計算を考え直す

連続する2要素 `a` と `b` に対する一段のチェックサムは `a .^. b .^. 1` と表せた。
ここから、連続する4要素 `a`～`d` に対する２段のチェックサムは
`(a .^. b .^. 1) .^. (c .^. d .^. 1) .^. 1 = a .^. b .^. c .^. d .^. 1`
となる。

つまり、長さが奇数になるまでチェックサムの計算を繰り返すとは、
長さのtrailing zeroの個数の値列のxorをとって、最後に否定をとったもの、とわかる。

```haskell
defrate3 :: Int -> [Int] -> [Int]
defrate3 sz xs = take cslen $ map ((1 .^.) . (1 .&.) . sum) $ chunksOf len xs
  where
    len = sz .^. pred sz
    cslen = div sz len

solve3 cs sz = map intToDigit . defrate3 sz . infrate2 sz . map digitToInt $ cs
```

結果：

```
ghci> main1b
"10011010010010010"
(0.64 secs, 351,996,872 bytes)
ghci> main2b
"10101011110100011"
(14.54 secs, 14,118,332,760 bytes)
```

きっとこれが最速。
