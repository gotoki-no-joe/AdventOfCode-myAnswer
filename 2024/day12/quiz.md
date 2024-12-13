# 12日目: ガーデングループ

[庭師](../../2023/day5/quiz.md)と彼の[広大な農場](../../2023/day21/quiz.md)の近くで主任歴史学者を探してみませんか？
食べ物はたくさんあるので、歴史学者たちは探しがてら何かを食べています。

あなたは複雑な庭の区画の近くに落ち着こうとしていると、妖精たちが手を貸してくれないかと尋ねてきます。
彼らは庭の区画のそれぞれの周りに柵を設置したいのですが、どれだけの柵を注文する必要があるのか、
またその費用がどれくらいになるのかがわかりません。
彼らはあなたに庭の区画の地図（あなたのパズルの入力）を渡します。

庭のそれぞれの区画は単一の種類の植物のみを育てており、地図上では1文字で示されています。
庭の複数の区画が同じ種類の植物を育てていて、（水平または垂直に）接触している場合、それらは**領域**を成します。
例えば：

```
AAAA
BBCD
BBCC
EEEC
```

この4x4の配置には、（A, B, C, D, Eとラベル付けされた）5種類の植物を育てる庭の区画が含まれており、
それぞれが独自の領域に集まっています。

ある領域の柵のコストを正確に計算するためには、その領域の**面積**と**周囲の長さ**を知る必要があります。

領域の**面積**は、その領域に含まれる庭の区画の数です。
上の地図の種類 A, B, C の植物はそれぞれ面積4の領域にあります。
種類 E の植物は面積3の領域にあり、種類 D の植物は面積1の領域にあります。

庭の区画はどれも正方形であり、したがって**4つの辺**があります。
領域の**周囲の長さ**は、庭の区画の辺で、その領域内の他の区画と接していないものの数です。
種類 A および C の植物は、それぞれ周長が 10 の領域にあります。
種類 B および E の植物は、それぞれ周長が 8 の領域にあります。
孤立した D の区画はそれ自身で領域をなし、周長は 4 です。

上記の地図の領域の周囲は、次のように `-` と `|` を用いて、
各領域の区画の辺を視覚的に示すことによって、測定されます：

```
+-+-+-+-+
|A A A A|
+-+-+-+-+     +-+
              |D|
+-+-+   +-+   +-+
|B B|   |C|
+   +   + +-+
|B B|   |C C|
+-+-+   +-+ +
          |C|
+-+-+-+   +-+
|E E E|
+-+-+-+
```

同じ種類の植物は複数の別々の領域に現れることがあり、領域は他の領域の中に現れることさえあります。
例えば：

```
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
```

上の地図には**5つ**の領域が含まれており、そのうちの1つは全てのO型庭の区画を含み、
他の4つはそれぞれ単一のX型区画を含んでいます。

4つのX領域はそれぞれ面積1、周長4を持っています。
21個のO型植物を含む領域はより複雑です。
外側の縁が周長に20寄与するだけでなく、
個々のX領域との境界が周長にさらに4を加え、周長は合計36になります。

「現代的な」商慣行のため、領域に必要な柵の価格は、その領域の面積に周長を**掛ける**ことで求められます。
地図上の全ての領域の柵の**総価格**は、地図上の全ての領域の柵の価格を合計することで求められます。

最初の例では、領域 A の価格は $4 \times 10 = 40$、
領域 B の価格は $4 \times 8 = 32$、領域 C の価格は $4 \times 10 = 40$、
領域 D の価格は $1 \times 4 = 4$、領域 E の価格は $3 \times 8 = 24$ です。
したがって、最初の例の総価格は140です。

2番目の例では、全てのO植物がある領域の価格は $21 \times 36 = 756$ で、
4つの小さな X 領域のそれぞれの価格は $1 \times 4 = 4$ で、
総価格は 772 $(756 + 4 + 4 + 4 + 4)$ です。

こちらはより大きな例です：

```
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
```

内訳は次の通りです：

- R 植物の領域の価格は $12 \times 18 = 216$
- I 植物の領域の価格は $4 \times 8 = 32$
- C 植物の領域の価格は $14 \times 28 = 392$
- F 植物の領域の価格は $10 \times 18 = 180$
- V 植物の領域の価格は $13 \times 20 = 260$
- J 植物の領域の価格は $11 \times 20 = 220$
- C 植物の領域の価格は $1 \times 4 = 4$
- E 植物の領域の価格は $13 \times 18 = 234$
- I 植物の領域の価格は $14 \times 22 = 308$
- M 植物の領域の価格は $5 \times 12 = 60$
- S 植物の領域の価格は $3 \times 8 = 24$

したがって、合計価格は1930です。

あなたの地図上の全ての領域の柵の合計価格はいくらですか？

# パート2

幸いなことに、妖精たちは大量の柵を注文しようとしているので、**まとめ買い割引**の対象になります！

まとめ買い割引では、価格を計算するために周長を使用するのではなく、
各領域が持つ**辺の数**を使用する必要があります。
一本の真っ直ぐな柵は、その長さに関係なく、1つの辺として数えます。

この例を再度考えてみましょう：

```
AAAA
BBCD
BBCC
EEEC
```

種類 A の植物を含む領域は4つの辺を持ち、種類 B, D, E の植物を含む領域もそれぞれ同様です。
しかし、種類 C の植物を含むかなり複雑な領域は8つの辺を持っています！

領域ごとの価格を領域の面積に辺の数を掛けて計算する新しい方法を使用すると、
領域 A から E の価格はそれぞれ 16, 16, 32, 4, 12 となり、合計価格は 80 になります。

上記の2番目の例（種類 X と O の植物でいっぱいの）は、合計価格436になります。

以下は、種類 E の植物でいっぱいのE字型の領域を含む地図です：

```
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
```

E字型の領域は、面積17と辺12を持ち、価格は204となります。
X 型植物で満たされた2つの領域を含めて、この地図の合計価格は236になります。

次の地図の合計価格は368です：

```
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
```

種類 B の植物が生い茂る2つの領域（それぞれ4辺）と、
種類 A の植物が生い茂る1つの領域（外側に4辺、内側にさらに8辺、合計12辺）があります。
種類 A の植物の領域を囲う柵を数えるときには特別な注意を払う必要があります。
柵の各部分には内側と外側があるので、
柵は領域の真ん中（2つの B 領域が斜めに接するところ）を横切って繋げることはできません。
（妖精たちがメビウス柵製作所と契約していればよかったのですが、
彼らの契約条件はあまりにも一方的でした。）

以前の大きな例では、現在以下のように価格が更新されています：

- R 植物の領域は価格 $12 \times 10 = 120$ です。
- I 植物の領域は価格 $4 \times 4 = 16$ です。
- C 植物の領域は価格 $14 \times 22 = 308$ です。
- F 植物の領域は価格 $10 \times 12 = 120$ です。
- V 植物の領域は価格 $13 \times 10 = 130$ です。
- J 植物の領域は価格 $11 \times 12 = 132$ です。
- C 植物の領域は価格 $1 \times 4 = 4$ です。
- E 植物の領域は価格 $13 \times 8 = 104$ です。
- I 植物の領域は価格 $14 \times 16 = 224$ です。
- M 植物の領域は価格 $5 \times 6 = 30$ です。
- S 植物の領域は価格 $3 \times 6 = 18$ です。

これらを合計すると、新しい合計価格は1206になります。

地図上の全ての領域の柵の新しい合計価格はいくらですか？