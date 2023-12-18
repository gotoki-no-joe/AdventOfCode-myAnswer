# 5日目: 種に肥料を与えたら

あなたはボートに乗り、言われた場所で庭師を見つけます。
彼は農場のように見える巨大な「庭」を管理しています。

「水源？ここ島之島**が**水源だよ！」
あなたは、雪之島には水が供給されていないと指摘しました。

「ああ、濾過に使う**砂がなくなってしまった**ので、水を止めなければならなかったのです。
汚れた水では雪は作れません。心配しないでください、すぐにもっと砂が手に入ると思います。
水は少しの間止めるだけです、数日…数週間…ああ、だめだ。」
彼の顔が恐怖に気づいたような表情に沈みます。

「ここにいるみんなに食べ物を届けるのに忙しくて、
なぜ砂が増えなくなったのかを確認するのをすっかり忘れていました！
そっちの方面に向かう渡し船がもうすぐ出発します。
あなたのボートよりもはるかに速いです。行って調べてもらえませんか？」

あなたがそれに同意するかしないかのうちにまた、彼はまた別の要求を持ち出しました。<!-- barely超訳 -->
「渡し船を待っている間に、私たちの**食糧生産問題**を手伝ってください。
最新の島之島年鑑が届いたばかりですが、私たちはそれを理解するのに苦労しています。」

年鑑（あなたのパズル入力）には、植える必要があるすべての種子(seeds)がリストされています。
また、各種類の種子にどのような種類の土壌(fertilizer)を使用するか、
各種類の土壌にどのような種類の肥料(soil)を使用するか、
各種類の肥料に使用する水(water)の種類などもリストされています。
種子、土壌、肥料などのあらゆる種類は番号で識別されますが、番号はカテゴリごとに再利用されます。
つまり、土壌123と肥料123は必ずしも相互に関連しているわけではありません。

例えば：

```
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
```

この年鑑は、どの種子を植える必要があるかのリストから始まります: 種子 79, 14, 55, 13。

年鑑の残りの部分には、変換**元カテゴリ**の数値を変換**先カテゴリ**の数値に変換する方法を説明する**写像**のリストが含まれています。
つまり、`seed-to-soil map:` から始まる節では、**種子番号**（元）を**土壌番号**（先）に変換する方法が説明されています。
これにより、庭師とそのチームは、どの土壌をどの種子に使用するか、どの水をどの肥料とともに使用するかなどを知ることができます。

全ての元番号とそれに対応する先番号をひとつずつ列挙するのではなく、写像は変換可能な番号の**範囲**全体を記述します。
写像の各行には、**先範囲の開始点**、**元範囲の開始点**、**範囲の長さ**の3つの数値が含まれます。

もう一度`seed-to-soil map`の例を考えてみましょう。

```
50 98 2
52 50 48
```

最初の行は、**先範囲の開始点**が50、**元範囲の開始点**が98、**範囲の長さ**が2です。
この行は、元範囲が98で始まり、98,99のふたつの値を含むことを意味します。
先範囲は同じ長さですが、50から始まるため、そのふたつの値は50と51です。
この情報により、種子番号98が土壌番号50に対応し、種子番号99が土壌番号51に対応することがわかります。

2行目は、元範囲が50で始まり、48個の値を含むことを意味します。これは50,51,…,96,97です。
これが、52で始まりやはり48個の値を持つ先範囲52,53,…,98,99に対応します。
したがって、種子番号53は土壌番号55に対応します。

**対応付けされていない**元番号は全て、**同じ**先番号に対応します。
したがって、種子番号10は土壌番号10に対応します。

したがって、種子番号とそれに対応する土壌番号のリスト全体は次のようになります。

```
seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51
```

この写像を使用すると、スタート位置であるそれぞれの種子番号に必要な土壌番号を調べることができます。

- 種子番号79は土壌番号81に対応します。
- 種子番号14は土壌番号14に対応します。
- 種子番号55は土壌番号57に対応します。
- 種子番号13は土壌番号13に対応します。

庭師とそのチームはできるだけ早く作業を開始したいと考えており、種子が必要な最寄りの場所を知りたいと考えています。
これらの写像を使用して、**いずれかの種子に対応する最小の場所番号**を見つけてください。
これを行うには、対応する場所番号が見つかるまで、他のカテゴリを通じてそれぞれの種子番号を変換する必要があります。
この例では、対応する種類は次のとおりです：

- 種79、土81、肥料81、水81、光74、温度78、湿度78、**場所82**。
- 種14、土14、肥料53、水49、光42、温度42、湿度43、**場所43**。
- 種55、土57、肥料57、水53、光46、温度82、湿度82、**場所86**。
- 種13、土13、肥料52、水41、光34、温度34、湿度35、**場所35**。

したがって、この例の最小の場所番号は**35**です。

**いずれかの種子番号に対応する最小の場所番号は何ですか？**

# パート2

そんなに少ない数の種を植えただけでは、誰もが飢えてしまいます。
年鑑を読み直してみると、`seeds:`の行は実際には**種子番号の範囲**を記述しているようです。

最初の`seeds:`の行の値は2つ1組で考えます。
それぞれの組は、最初の値は範囲の**開始**位置、2番目の値は範囲の**長さ**です。
したがって、上記の例の最初の行：

```
seeds: 79 14 55 13
```

この行は、庭に植える種子の番号の2つの範囲を説明します。
最初の範囲は種子番号79で始まり14個の値79, 80, …, 91, 92を持ちます。
2番目の範囲は種子番号55で始まり、13個の値55, 56, …, 66, 67を持ちます。

今や、4つの種子番号だけを考えるのではなく、合計27の種子番号を考える必要があります。

上記の例では、最小の場所番号は、種子番号82から得られます。
これは土壌82、肥料82、水82、光77、温度45、湿度46、そして**場所46**に対応しています。
したがって、最小の場所番号は**46**です。

年鑑の最初の行に範囲で列挙されている全ての種子番号を考慮してください。
**いずれかの種子番号に対応する最小の場所番号は何ですか？**