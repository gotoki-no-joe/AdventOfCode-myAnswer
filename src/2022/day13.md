# 13日目: 救難信号

丘を登り、再び小人との接触を試みます。
ところが、代わりに、予期していなかった信号を受信します：**遭難信号**です。

携帯機器はまだ正常に動作していないようです。
遭難信号からのパケットが**間違った順序**でデコードされました。
メッセージを復号するには、受信したパケットのリスト（パズルの入力）を並べ替える必要があります。

リストはパケットの対で構成されています。対は空白行で区切られます。
**正しい順序になっているパケットの対の個数**を特定する必要があります。

例えば：

```
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
```

パケットデータは、リストと整数で構成されます。
各リストは `[` で始まり `]` で終わり、0個以上のカンマ区切りの値（整数または別のリスト）を含みます。
各パケットは常にリストであり、一行にひとつが示されています。

2つの値を比較するとき、ひとつめの値を**左**と呼び、ふたつめの値を**右**と呼びます。
そして：

- **両方の値が整数**である場合、**小さい方の整数**が先にくる必要があります。
左の整数が右の整数より小さい場合、入力は正しい順序です。
左の整数が右の整数より大きい場合、入力は正しい順序ではありません。
さもなくば、入力は同じ整数です。入力の次の部分のチェックを続けます。
- **両方の値がリスト**である場合は、
各リストの最初の値を比較し、次に2番目の値を比較し、以下同様に続けます。
左のリストが先に要素を使い果たした場合、入力は正しい順序です。
右のリストが先に要素を使い果たした場合、入力は正しい順序ではありません。
リストが同じ長さで、比較によって順序が決定されない場合は、入力の次の部分のチェックを続けます。
- いずれか**一方の値だけが整数**である場合は、
その整数を唯一の値として含むリストに変換してから、比較を再試行します。
例えば、`[0,0,0]` と `2` を比較する場合、右の値を `[2]` （`2`を持つリスト）に変換します。
比較の結果は代わりに `[0,0,0]` と `[2]` を比較することで得られます。

これらの規則を用いて、例のどの対が正しい順序であるかを判断できます。

```
== Pair 1 ==
- Compare [1,1,3,1,1] vs [1,1,5,1,1]
  - Compare 1 vs 1
  - Compare 1 vs 1
  - Compare 3 vs 5
    - Left side is smaller, so inputs are in the right order
訳：
- 比較 [1,1,3,1,1] vs [1,1,5,1,1]
  - 比較 1 vs 1
  - 比較 1 vs 1
  - 比較 3 vs 5
    - 左が小さいので、入力は正しい順序

== Pair 2 ==
- Compare [[1],[2,3,4]] vs [[1],4]
  - Compare [1] vs [1]
    - Compare 1 vs 1
  - Compare [2,3,4] vs 4
    - Mixed types; convert right to [4] and retry comparison
      種類が混ざっているので、右を [4] に変換して再比較
    - Compare [2,3,4] vs [4]
      - Compare 2 vs 4
        - Left side is smaller, so inputs are in the right order
          左が小さいので、入力は正しい順序

== Pair 3 ==
- Compare [9] vs [[8,7,6]]
  - Compare 9 vs [8,7,6]
    - Mixed types; convert left to [9] and retry comparison
    - Compare [9] vs [8,7,6]
      - Compare 9 vs 8
        - Right side is smaller, so inputs are not in the right order

== Pair 4 ==
- Compare [[4,4],4,4] vs [[4,4],4,4,4]
  - Compare [4,4] vs [4,4]
    - Compare 4 vs 4
    - Compare 4 vs 4
  - Compare 4 vs 4
  - Compare 4 vs 4
  - Left side ran out of items, so inputs are in the right order
    左の要素が尽きたので、入力は正しい順序

== Pair 5 ==
- Compare [7,7,7,7] vs [7,7,7]
  - Compare 7 vs 7
  - Compare 7 vs 7
  - Compare 7 vs 7
  - Right side ran out of items, so inputs are not in the right order

== Pair 6 ==
- Compare [] vs [3]
  - Left side ran out of items, so inputs are in the right order

== Pair 7 ==
- Compare [[[]]] vs [[]]
  - Compare [[]] vs []
    - Right side ran out of items, so inputs are not in the right order

== Pair 8 ==
- Compare [1,[2,[3,[4,[5,6,7]]]],8,9] vs [1,[2,[3,[4,[5,6,0]]]],8,9]
  - Compare 1 vs 1
  - Compare [2,[3,[4,[5,6,7]]]] vs [2,[3,[4,[5,6,0]]]]
    - Compare 2 vs 2
    - Compare [3,[4,[5,6,7]]] vs [3,[4,[5,6,0]]]
      - Compare 3 vs 3
      - Compare [4,[5,6,7]] vs [4,[5,6,0]]
        - Compare 4 vs 4
        - Compare [5,6,7] vs [5,6,0]
          - Compare 5 vs 5
          - Compare 6 vs 6
          - Compare 7 vs 0
            - Right side is smaller, so inputs are not in the right order
```

**正しい順序になっている**対の番号は何ですか？
（最初の対の番号は1、2番目の対の番号は2というように続きます。）
上記の例では、正しい順序の対は1, 2, 4, 6 です。
これらの番号の合計は**13**です。

どのパケットの対が正しい順序になっているかを判定します。
**それらの対の番号の合計はいくつですか？**

<!--
<details><summary>解説</summary><div>

このパケット、すなわち何段でも入れ子にできる整数のリストを表すデータ型を定義する。
順序をもつので `Ord` 型クラスに入れるために、`Eq` は自動導出させる。
整数とリストの比較が独特なので、`Ord` は自動導出できない。
また、見た目どおりに表示するように `Show` に入れる（オプション）。

```haskell
data Packet = PInt Int | PList [Packet] deriving Eq

instance Ord Packet where
  compare (PInt x) (PInt y) = compare x y
  compare (PList xs) (PList ys) = compare xs ys
  compare x@(PInt _) (PList ys) = compare [x] ys
  compare (PList xs) y@(PInt _) = compare xs [y]

instance Show Packet where
  showsPrec p (PInt n) = showsPrec p n
  showsPrec p (PList ps) = showChar '[' . foldr (.) (showChar ']') (intersperse (showChar ',') (map shows ps))
```

パーサは `Text.Parsec` を用いて手早く作る。

```haskell
import Text.Parsec
import Text.Parsec.String

pPacket :: Parser Packet
pPacket =
  PInt . read <$> many1 digit
  <|>
  PList <$> between (char '[') (char ']') (Text.Parsec.sepBy parsePacket (char ','))
```

あとは計算するだけ。下から上に読むワンライナー。

```haskell
import Data.List.Split (chunksOf)

phase1a fn =
  print . sum . map fst .                        -- その順序を取り出し、合計して、出力
  filter (\(i,[l,r]) -> l < r) .                 -- 正しい順序のものだけ取り出し
  zip [1..] .                                    -- 番号を付け
  chunksOf 2 .                                   -- 対ごとにリストに切り分け
  map (fromRight undefined . parse pPacket "") . -- それぞれパーサにかけて、Rightを外し
  filter (not . null) .                          -- 空行を飛ばし
  lines =<<                                      -- 行ごとに分け
  readFile fn                                    -- ファイルを読み込み
```

</div></details>
-->

# パート2

あとは、**全ての**パケットを正しい順序に並べるだけです。
受信したパケットのリストの空白行は無視してください。

遭難信号プロトコルは、2つの追加の**分割パケット**（下記）を含めることも必要です。

```
[[2]]
[[6]]
```

前と同じ規則を使用して、全てのパケット ー
受信したパケットのリストにあるパケットと2つの分割パケット ー
を正しい順序に揃えます。

上記の例では、パケットを正しい順序に並べた結果は次のようになります。

```
[]
[[]]
[[[]]]
[1,1,3,1,1]
[1,1,5,1,1]
[[1],[2,3,4]]
[1,[2,[3,[4,[5,6,0]]]],8,9]
[1,[2,[3,[4,[5,6,7]]]],8,9]
[[1],4]
[[2]]                       <---
[3]
[[4,4],4,4]
[[4,4],4,4,4]
[[6]]                       <---
[7,7,7]
[7,7,7,7]
[[8,7,6]]
[9]
```

その後、分割パケットを見つけます。
この救難信号の**復号鍵**を見つけるには、2つの分割パケットの位置を求め、それらを乗算する必要があります。
（最初のパケットは位置1、2番目のパケットは位置2、と続きます。）
この例では、分割パケットは**10番目**と**14番目**であるため、復号鍵は**140**です。

全てのパケットを正しい順序に揃えてください。
**遭難信号の復号鍵はいくつですか？**

<!--
<details><summary>解説</summary><div>

パケット列に直すところまでは上と同じで、必要な計算を続ける。

```haskell
import Data.List

phase2 fn =
  do
    packets <-map (fromRight undefined . parse pPacket "") . filter (not . null) . lines <$> readFile fn
    let ps = sort $ div1 : div2 : packets
    let Just l1 = elemIndex div1 ps
    let Just l2 = elemIndex div2 ps
    print $ succ l1 * succ l2

Right div1 = parse parsePacket "" "[[2]]"
Right div2 = parse parsePacket "" "[[6]]"
```

よく考えてみると、\\(O(\log N)\\) かけてソートをする必要すらなくて、
分割パケットよりも小さいパケットの個数を2度数えるだけで、
\\(O(N)\\)で答えは算出できる。

```haskell
import Data.List

phase2 fn =
  do
    packets <-map (fromRight undefined . parse pPacket "") . filter (not . null) . lines <$> readFile fn
    let ps = div1 : div2 : packets
    let l1 = length $ filter (div1 >=) ps
    let l2 = length $ filter (div2 >=) ps
    print $ l1 * l2

Right div1 = parse parsePacket "" "[[2]]"
Right div2 = parse parsePacket "" "[[6]]"
```

</div></details>
-->
