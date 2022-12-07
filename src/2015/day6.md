# 6日目：火災の危険性あり

あなたのご近所さんがホリデーハウスデコレーションコンテストであなたを
毎年敗北させ続けるので、
\\(1,000 \times 1,000\\) グリッドに 100万本のライトを配備することに決めました。

さらに、あなたは今年、特にいい子でいたので、
理想的な照明を表示する構成の手順書をサンタからメールで受け取りました。

グリッド内のライトは、各方向に0から999の番号が付けられます。
四隅のライトはそれぞれ座標`0,0`、`0,999`、`999,999`、`999,0`となります。
各指示は
`turn on`（点灯）、`turn off`（消灯）、`toggle`（反転）
のいずれかです。
指示には、座標の対によりその操作を行う範囲（境界を含む）が含まれます。
各座標対は、長方形の対向する角を表します。
よって例えば `0,0 through 2,2` という座標対は、\\(3 \times 3\\) の正方形の9つのライトを指します。
すべてのライトはオフで始まります。

今年あなたのご近所さんに打ち勝つためには、
サンタから送られた手順書に従って、
あなたのライトを設定しさえすればよいのです。

例えば：

- `turn on 0,0 through 999,999` はすべてのライトをオンにします。
（元からオンのものはオンのままです）。
- `toggle 0,0 through 999,0` 第1行の1000個のライトを反転します。
オンになっていたものをオフにし、オフになっていたものをオンにします。
- `turn off 499,499 through 500,500` 中央の4つのライトをオフにします。
（元からオフのものはオフのままです。）

一通り指示書に従った後、**点灯しているライトはいくつですか？**

<details><summary>解説</summary><div>

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

入力データを読み込んで指示のリストに変換したら、\\(1000 \times 1000\\) の配列を作り、
指示通りにライトを操作して、最終結果を取り出せばよい。
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

</div></details>

# パート2

勝利パターンを実装し終わったとき、サンタからのメッセージを
古代ノルディック小人語から翻訳するときに間違えていたことに気付きました。

あなたが購入したライトグリッドには、実際には個々の明るさコントロールがありました。
各ライトは0以上の明るさを持ちます。ライトはすべて明るさ0で始まります。

`turn on`というフレーズは実際には
あなたがそれらのライトの明るさを1増やすべきであることを意味します。

`turn off`というフレーズは実際には
あなたがライトの明るさを1下げる必要があることを意味します。
その最小値は零です。

`toggle`のフレーズは実際には
あなたがそれらのライトの明るさを2増やすべきであることを意味します。

サンタの指示に従った後に、すべてのライトの明るさを合わせた
**明るさの合計値**はいくつでしょう？

例えば：

- `turn on 0,0 through 0,0`は合計値を1増加させます。
- `toggle 0,0 through 999,999`は合計値を2000000増加させます。

<details><summary>解説</summary><div>

パート1では2値だったライトの状態を整数にし、指示に対する操作を定義通りに差し替えるだけ。

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

ちなみに、「二次元累積和」という手法を使うと、この問題の答えをずっと効率的に求めることができる。

</div></details>
