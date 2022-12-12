# 11日目: サル・イン・ザ・ミドル（中間猿攻撃）

ようやく川を遡り始めると、リュックが思ったよりもずっと軽いことに気づきます。
ちょうどその時、リュックの荷物のひとつが頭上を飛んでいきます。
サルはあなたの無くし物で[Keep Away](https://en.wikipedia.org/wiki/Keep_away)をして遊んでいます！

荷物を取り戻すには、サルがどこに荷物を投げるかを予測できる必要があります。
注意深い観察の結果、サルは**それぞれの荷物に対するあなたの心配度**に基づいて行動していることに気付きました。

それぞれのサルが現在持っている荷物、それらの荷物に対するあなたの心配度、
心配度に基づいてサルがどのように決定を下すかについて、
あなたは何やらメモを取りました（パズル入力）。
例えば：

```
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
```

それぞれのサルはいくつかの属性を持ちます。

- `Starting items` サルが現在持っている荷物のそれぞれの**心配度**の、サルがそれを検分する順番のリストです。
- `Operation` サルが荷物を検分することで、あなたの心配度がどのように変化するかを示しています。
（例えば `new = old * 5` とあるとき、
サルが荷物を検分した後の心配度が、検査前の心配度の5倍になることを意味します。）
- `Test` は、サルがあなたの心配度を使用して、次にどこに荷物を投げるかを決定する方法を示しています。
  - `If true` テストが真になるときに荷物がどうなるかを示します。
  - `If false` テストが偽の場合に荷物がどうなるかを示します。

サルが荷物を検分した後、あなたの心配度に対してTestをする前に、
サルの検分が荷物を駄目にせずに済んだというあなたの安堵が、
あなたの心配度を**3で割り**、最も近い整数に切り捨てます。
（訳注：「近い」でなく床関数での切り捨て）

サルは順に荷物を検分して投げます。
あるサルの番では、そのサルが持っている全ての荷物を一度にひとつずつ、リストにある順に、
検分しては投げます。
サル`0`が最初に、次にサル`1`というように、全てのサルに順番が回るまで続けます。
全てのサルに一度順番が回る過程を**ラウンド**（一巡）と呼びます。

サルが別のサルに荷物を投げると、その荷物は受け取ったサルのリストの**末尾**に入ります。
あるサルが開始時にリストに荷物がない状態であったとしても、
順番が回ってきたときには検分して投げる荷物を大量に持っていることになる可能性があります。
サルが自分の番の開始時に荷物を持っていない場合、そのサルの番はそれで終わりです。

上記の例では、最初のラウンドは次のように進行します。

```
Monkey 0:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by 19 to 1501.
    Monkey gets bored with item. Worry level is divided by 3 to 500.
    Current worry level is not divisible by 23.
    Item with worry level 500 is thrown to monkey 3.
訳：
サル 0:
  サルが心配度79の荷物を検分します。
  心配度に19を掛けて1501になります。
  サルが荷物に飽きました。心配度は3で割って500になります。
  現在の心配度は23で割り切れません。
  心配度500の荷物がサル3に投げられます。
```
続き
```
  Monkey inspects an item with a worry level of 98.
    Worry level is multiplied by 19 to 1862.
    Monkey gets bored with item. Worry level is divided by 3 to 620.
    Current worry level is not divisible by 23.
    Item with worry level 620 is thrown to monkey 3.
Monkey 1:
  Monkey inspects an item with a worry level of 54.
    Worry level increases by 6 to 60.
    Monkey gets bored with item. Worry level is divided by 3 to 20.
    Current worry level is not divisible by 19.
    Item with worry level 20 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 65.
    Worry level increases by 6 to 71.
    Monkey gets bored with item. Worry level is divided by 3 to 23.
    Current worry level is not divisible by 19.
    Item with worry level 23 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 75.
    Worry level increases by 6 to 81.
    Monkey gets bored with item. Worry level is divided by 3 to 27.
    Current worry level is not divisible by 19.
    Item with worry level 27 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 74.
    Worry level increases by 6 to 80.
    Monkey gets bored with item. Worry level is divided by 3 to 26.
    Current worry level is not divisible by 19.
    Item with worry level 26 is thrown to monkey 0.
Monkey 2:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by itself to 6241.
    Monkey gets bored with item. Worry level is divided by 3 to 2080.
    Current worry level is divisible by 13.
    Item with worry level 2080 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 60.
    Worry level is multiplied by itself to 3600.
    Monkey gets bored with item. Worry level is divided by 3 to 1200.
    Current worry level is not divisible by 13.
    Item with worry level 1200 is thrown to monkey 3.
  Monkey inspects an item with a worry level of 97.
    Worry level is multiplied by itself to 9409.
    Monkey gets bored with item. Worry level is divided by 3 to 3136.
    Current worry level is not divisible by 13.
    Item with worry level 3136 is thrown to monkey 3.
Monkey 3:
  Monkey inspects an item with a worry level of 74.
    Worry level increases by 3 to 77.
    Monkey gets bored with item. Worry level is divided by 3 to 25.
    Current worry level is not divisible by 17.
    Item with worry level 25 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 500.
    Worry level increases by 3 to 503.
    Monkey gets bored with item. Worry level is divided by 3 to 167.
    Current worry level is not divisible by 17.
    Item with worry level 167 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 620.
    Worry level increases by 3 to 623.
    Monkey gets bored with item. Worry level is divided by 3 to 207.
    Current worry level is not divisible by 17.
    Item with worry level 207 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 1200.
    Worry level increases by 3 to 1203.
    Monkey gets bored with item. Worry level is divided by 3 to 401.
    Current worry level is not divisible by 17.
    Item with worry level 401 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 3136.
    Worry level increases by 3 to 3139.
    Monkey gets bored with item. Worry level is divided by 3 to 1046.
    Current worry level is not divisible by 17.
    Item with worry level 1046 is thrown to monkey 1.
```

第1ラウンドの後、サルはそれぞれ次の心配度の荷物を持っています：

```
Monkey 0: 20, 23, 27, 26
Monkey 1: 2080, 25, 167, 207, 401, 1046
Monkey 2: 
Monkey 3: 
```

サル`2`と`3`は、このラウンドの終了時に荷物を持っていません。
彼らはどちらもラウンド中に全ての荷物を検分し、ラウンドが終了する前にそれらを全て投げました。

この過程は、さらに数ラウンド続きます。

```
After round 2, the monkeys are holding items with these worry levels:
第2ラウンドの後にサルたちが持つ荷物の心配度：
Monkey 0: 695, 10, 71, 135, 350
Monkey 1: 43, 49, 58, 55, 362
Monkey 2: 
Monkey 3: 

After round 3, the monkeys are holding items with these worry levels:
Monkey 0: 16, 18, 21, 20, 122
Monkey 1: 1468, 22, 150, 286, 739
Monkey 2: 
Monkey 3: 

After round 4, the monkeys are holding items with these worry levels:
Monkey 0: 491, 9, 52, 97, 248, 34
Monkey 1: 39, 45, 43, 258
Monkey 2: 
Monkey 3: 

After round 5, the monkeys are holding items with these worry levels:
Monkey 0: 15, 17, 16, 88, 1037
Monkey 1: 20, 110, 205, 524, 72
Monkey 2: 
Monkey 3: 

After round 6, the monkeys are holding items with these worry levels:
Monkey 0: 8, 70, 176, 26, 34
Monkey 1: 481, 32, 36, 186, 2190
Monkey 2: 
Monkey 3: 

After round 7, the monkeys are holding items with these worry levels:
Monkey 0: 162, 12, 14, 64, 732, 17
Monkey 1: 148, 372, 55, 72
Monkey 2: 
Monkey 3: 

After round 8, the monkeys are holding items with these worry levels:
Monkey 0: 51, 126, 20, 26, 136
Monkey 1: 343, 26, 30, 1546, 36
Monkey 2: 
Monkey 3: 

After round 9, the monkeys are holding items with these worry levels:
Monkey 0: 116, 10, 12, 517, 14
Monkey 1: 108, 267, 43, 55, 288
Monkey 2: 
Monkey 3: 

After round 10, the monkeys are holding items with these worry levels:
Monkey 0: 91, 16, 20, 98
Monkey 1: 481, 245, 22, 26, 1092, 30
Monkey 2: 
Monkey 3: 

...

After round 15, the monkeys are holding items with these worry levels:
Monkey 0: 83, 44, 8, 184, 9, 20, 26, 102
Monkey 1: 110, 36
Monkey 2: 
Monkey 3: 

...

After round 20, the monkeys are holding items with these worry levels:
Monkey 0: 10, 12, 14, 26, 34
Monkey 1: 245, 93, 53, 199, 115
Monkey 2: 
Monkey 3: 
```

一度に全てのサルを追跡することは不可能です。
荷物を取り戻す望みを繋ぐには、**最も活発な2匹**のサルに集中する必要があります。
20ラウンドにわたってそれぞれのサルが**荷物を検分した合計回数**を数えます。

- サル0は荷物を101回検分しました
- サル1は荷物を 95回検分しました
- サル2は荷物を  7回検分しました
- サル3は荷物を105回検分しました

この例では、最も活発な2匹のサルが荷物を101回と105回検査しました。
この状況でのいたずら（**monkey business**）の度合いは、
これらを乗算することで**10605**と求めることができます。

サルが20ラウンドにわたって検分した荷物の数を数えて、どのサルを追いかけるべきかを特定します。
**20ラウンドの物投げサルの悪ふざけの後、いたずらの度合いはいくつですか？**
（原文 stuff-slinging simian shenanigans 物を投げるサルのいたずら）

<!--
<details><summary>解説</summary><div>

実際のパズル入力でもサルはたかだか8匹なので、データはファイルから読み込みせずに手作業でプログラムに直す。
サルを表すデータ型を定義する。

```haskell
type WorryLevel = Int
data Monkey = Monkey
  { si  :: [WorryLevel]              -- Starting items
  , op  :: WorryLevel -> WorryLevel  -- Operation
  , tst :: WorryLevel                -- testの除数
  , t   :: Int                       -- 割り切れるときに投げる先
  , f   :: Int }                     -- 割り切れないときに投げる先

testData =
  [ Monkey [79, 98]         (19 *) 23 2 3
  , Monkey [54, 65, 75, 74] (6 +)  19 2 0
  , Monkey [79, 60, 97]     (^ 2)  13 1 3
  , Monkey [74]             (3 +)  17 0 1
  ]
```

激しく状態変化が起きるので、命令型言語が似合っている内容なのは否めない。
その近似として、IOモナドの中で、`IOArray` を用いて状況を表現する。
サルの持ち物リストは `Data.Sequence` で扱う。

```haskell
import Control.Monad
import Data.Array.IO
import qualified Data.Sequence as Q
import Data.Foldable
import Data.List

phase1 :: [Monkey] -> IO ()
phase1 ms =
  do
-- サルの持ち物リストの配列
    qv <- newListArray (0, nM) $ map (Q.fromList . si) ms :: IO (IOArray Int (Q.Seq WorryLevel))
-- 荷物を検分した回数カウンタ配列
    cv <- newArray (0, nM) 0 :: IO (IOArray Int Int)
-- 20ラウンド繰り返す
    forM_ [1..20] (\round -> do
-- それぞれのサルに順番を回す
      forM_ (zip [0..] ms) (\(i, m) -> do
-- 持ち物リストを読み出す
        q <- readArray qv i
-- これは全て投げるのでリストを空にする
        writeArray qv i Q.empty
-- 検分した回数を増やす
        readArray cv i >>= writeArray cv i . (Q.length q +)
-- それぞれの荷物について
        forM_ (toList q) (\item -> do
-- 心配度はサル固有の計算の後3で割る
            let item1 = div (op m item) 3
-- 割り切れるかどうかで送り先jを決める
            let j = if mod item1 (tst m) == 0 then t m else f m
-- そのサルの持ち物リストの末尾に追加する
            readArray qv j >>= writeArray qv j . (Q.|> item1)
            )
        )
-- (確認用) ラウンド終了後の持ち物リスト
      print ("round ", round)
      md <- getElems qv
      mapM_ print md
      )
-- いたずら度を算出
    cnts <- getElems cv
    print $ product $ take 2 $ sortBy (flip compare) cnts
  where
    nM = pred $ length ms
```

IOモナドですることで、デバッグ出力も容易にできた。

</div></details>
-->

# パート2

あなたは荷物を取り返せないのではないかと心配になってきました。
実際、猿の検分が荷物に損傷を与えなかったというあなたの安堵は、
**あなたの心配度が3で割られることをもはや引き起こさない**ほど心配しています。

残念ながら、その安堵は、あなたの心配度が**途方もないレベル**に達するのを防いでいた全てでした。
**心配度を管理できるようにする別の方法**を見つける必要があるでしょう。

この相場で、あなたはサルたちに**非常に長い間**、どうにか**10000ラウンド**我慢することになります。

これらの新しい規則で、10000ラウンド後のいたずら度を把握できます。
上記と同じ例を使用します。

- 第1ラウンドの後
  - サル0は荷物を2回検分しました
  - サル1は荷物を4回検分しました
  - サル2は荷物を3回検分しました
  - サル3は荷物を6回検分しました
- 20ラウンド後
  - サル0は荷物を 99回検分しました
  - サル1は荷物を 97回検分しました
  - サル2は荷物を  8回検分しました
  - サル3は荷物を103回検分しました
- 1000ラウンド後
  - サル0は荷物を5204回検分しました
  - サル1は荷物を4792回検分しました
  - サル2は荷物を 199回検分しました
  - サル3は荷物を5192回検分しました

```
== After round 2000 ==
Monkey 0 inspected items 10419 times.
Monkey 1 inspected items 9577 times.
Monkey 2 inspected items 392 times.
Monkey 3 inspected items 10391 times.

== After round 3000 ==
Monkey 0 inspected items 15638 times.
Monkey 1 inspected items 14358 times.
Monkey 2 inspected items 587 times.
Monkey 3 inspected items 15593 times.

== After round 4000 ==
Monkey 0 inspected items 20858 times.
Monkey 1 inspected items 19138 times.
Monkey 2 inspected items 780 times.
Monkey 3 inspected items 20797 times.

== After round 5000 ==
Monkey 0 inspected items 26075 times.
Monkey 1 inspected items 23921 times.
Monkey 2 inspected items 974 times.
Monkey 3 inspected items 26000 times.

== After round 6000 ==
Monkey 0 inspected items 31294 times.
Monkey 1 inspected items 28702 times.
Monkey 2 inspected items 1165 times.
Monkey 3 inspected items 31204 times.

== After round 7000 ==
Monkey 0 inspected items 36508 times.
Monkey 1 inspected items 33488 times.
Monkey 2 inspected items 1360 times.
Monkey 3 inspected items 36400 times.

== After round 8000 ==
Monkey 0 inspected items 41728 times.
Monkey 1 inspected items 38268 times.
Monkey 2 inspected items 1553 times.
Monkey 3 inspected items 41606 times.

== After round 9000 ==
Monkey 0 inspected items 46945 times.
Monkey 1 inspected items 43051 times.
Monkey 2 inspected items 1746 times.
Monkey 3 inspected items 46807 times.

== After round 10000 ==
Monkey 0 inspected items 52166 times.
Monkey 1 inspected items 47830 times.
Monkey 2 inspected items 1938 times.
Monkey 3 inspected items 52013 times.
```

10000ラウンド後、最も活発な 2 匹のサルは52166回と52013回、荷物を検分しました。
これらを掛け合わせると、この状況でのいたずら度は**2713310158**です。

荷物が検分された後に、心配度が3で割られることはなくなりました。
心配度を管理できるように保つ別の方法を見つける必要があります。
パズル入力の初期状態からやり直すと、
**10000ラウンド後のいたずら度はいくつですか？**

<!--
<details><summary>解説</summary><div>

ラウンド回数を指定できるように `phase1` を改造して実行すると結果が例と食い違う。
`Int`がオーバーフローしている。
`type WorryLevel = Integer` とすると、1000回の結果がいつまでも求められない。
心配度をそのまま扱うと、途方もない桁数になってしまう。

全てのサルが、投げる先の判定を「特定の数で割り切れるかどうか」で行っていることに注目する。
つまり、それらの数で割り切れる値かどうかだけが正しく判断できればよい。

そこで心配度を、
サル全員の割る数の最小公倍数（もしくは全ての掛けた数）を法にしたモジュロ整数
で扱うように修正する。`WorryLevel`は`Int`のままで構わない。

コメントを付けたところだけが `phase1` からの変更部分。

```haskell
phase2 :: [Monkey] -> Int -> IO ()
phase2 ms times =
  do
    qv <- newListArray (0, nM) $ map (Q.fromList . si) ms :: IO (IOArray Int (Q.Seq WorryLevel))
    cv <- newArray (0, nM) 0 :: IO (IOArray Int Int)
-- ラウンド回数は引数で指定
    forM_ [1..times] (\round -> do
      forM_ (zip [0..] ms) (\(i, m) -> do
        q <- readArray qv i
        readArray cv i >>= writeArray cv i . (Q.length q +)
        writeArray qv i Q.empty
        forM_ (toList q) (\item -> do
-- 心配度は3で割らないが、モジュロをとる
            let item1 = mod (op m item) base
            let j = if mod item1 (tst m) == 0 then t m else f m
            readArray qv j >>= writeArray qv j . (Q.|> item1)
            )
        )
      )
    cnts <- getElems cv
    print cnts
    print $ product $ take 2 $ sortBy (flip compare) cnts
  where
    nM = pred $ length ms
-- 法にするための最小公倍数を求める
    base = foldl1 lcm $ map tst ms
```

</div></details>
-->
