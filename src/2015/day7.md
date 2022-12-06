# 7日目：組み立てが少々必要

今年、サンタはボビー・テーブルス少年に
配線ワイヤとビット**論理ゲート**のセットを届けました！
残念ながら、Bobbyは推奨の年齢層より少し小さいので、
彼は回路を組み立てるのに助けが必要です。

各ワイヤは識別子（小文字アルファベット）を持ち、
16ビットの信号（0～65535の数）を伝えることができます。
各ワイヤには、ゲートまたは別のワイヤから信号が伝えられるか、定数値が与えられます。
各ワイヤは1つの信号源からのみ信号を得ることができますが、複数の接続先に信号を供給することができます。
ゲートは、その入力のすべてが信号を持つまで信号を供給しません。

付属の取扱説明書には、どのようにパーツを接続するかが書かれています。
`x AND y -> z`は
ワイヤ`x`とワイヤ`y`を`AND`ゲートに接続し、
その出力をワイヤ`z`に接続することを意味します。

例えば：

- `123 -> x` は信号`123`がワイヤ`x`に供給されることを意味します。
- `x AND y -> z` はワイヤ`x`とワイヤ`y`との**ビット単位の論理積**が
ワイヤ`z`に供給されることを意味します。
- `p LSHIFT 2 -> q` はワイヤ`p`からの値が2だけ**左シフト**され、
そのワイヤ`q`に提供されることを意味します。
- `NOT e -> f` はワイヤ`e`からの値の**ビット単位の反転**が
ワイヤ`f`に供給されることを意味します。

他の使用可能なゲートには、`OR`（**ビット単位OR**）および`RSHIFT`（**右シフト**）があります。
何らかの理由で代わりに回路を**エミュレート**したい場合、
ほとんどすべてのプログラミング言語（C, JavaScript, Pythonなど）はこれらのゲートの演算子を提供しています。

例えば、ここには簡単な回路があります：

~~~
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
~~~

それが実行された後、ワイヤ上の信号は次のようになります。

~~~
d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456
~~~
ボビー少年のキットの説明書
（あなたのパズル入力として提供されている）
では、最終的にどのような信号が**ワイヤ`a`に**流れますか？

<details><summary>解説</summary><div>

状況を整理すると、
アルファベット文字列で名前の付けられたワイヤ（信号線）があり、入力で与えられる回路結線で内容が定められる。
ループになっていたり、ひとつの信号に複数の入力が与えられたりはしていない。
結線は、他のワイヤの内容を読み取り、論理演算を施すもの。
正則な計算でこのような相互参照を解決するのは面倒な話だが、遅延評価を使うと非常に簡潔に解ける。
（ループや矛盾がないという保証がある場合に限る。）

すなわち、ワイヤ名を添え字として、その値を計算してコンテナに格納するが、
値を計算するために他のワイヤを読み取るとき、コンテナに値を問い合わせる。

ひとつの配線が3項組 `([String], [Word16] -> Word16, String)` で表されているとする。
第1要素は、参照しているワイヤ名、または定数の文字列のリスト。
第2要素は、参照した信号の内容を計算して、結果を返す演算を表す関数。
第3要素は、結果を供給する先のワイヤ名。
この情報のリストから、それぞれのワイヤに流れる信号を持つマップが次のように作れる。

```haskell
import qualified Data.Map as M

type Instr = ([String], [Word16] -> Word16, String)

part1Body :: [Instr] -> M.Map String Word16
part1Body is = m
  where
    m = M.fromList [(tgt, f $ map g ws) | (ws, f, tgt <- is)]
    g w
      | all isDigit w = read w
      | otherwise     = m M.! w
```

つまり、入力列から配線指示読み取ることができれば、`part1Body` の返すマップから答えは得られる。

```haskell
part1 :: [String] -> Word16
part1 ls = (part1Body $ map parse ls) M.! "a"
```

演算の名前がどこにあるのかは、語数からわかる。

```haskell
import Data.Bits

parse :: String -> Instr
parse xs =
  case length ws of
    3 -> ([ws !! 0], \[x] -> x, tgt)                  -- 123 -> x
    4 -> ([ws !! 1], \[x] -> complement x, tgt)       -- NOT x -> h
    5 -> ([ws !! 0, ws !! 2], getFun $ ws !! 1, tgt)  -- x AND y -> d
  where
    ws = words xs
    tgt = last ws

getFun "AND" = \[x,y] -> x .&. y
getFun "OR"  = \[x,y] -> x .|. y
getFun "LSHIFT" = \[x,y] -> shiftL x $ fromIntegral y
getFun "RSHIFT" = \[x,y] -> shiftR x $ fromIntegral y
```

</div></details>

# パート2 #

今度は、ワイヤ`a`上にある信号を取り出し、
その信号をワイヤ`b`へ上書きし、
他のワイヤ（ワイヤ`a`を含む）をリセットします。
最終的にどのような新しい信号がワイヤ`a`に流れますか？

<details><summary>解説</summary><div>

パート1の結果は手元にあるので、`part1Body` の中の `g` に細工をするのが手っ取り早い。

```haskell
part2 :: [String] -> Word16
part2 ls = m M.! "a"
  where
    m = M.fromList [(tgt, f $ map g ws) | (ws, f, tgt) <- map parse ls]
    g "b" = undefined -- パート1の結果をここに入れる
    g w
      | all isDigit w = read w
      | otherwise     = m M.! w
```

</div></details>
