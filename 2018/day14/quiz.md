# 14日目：チョコレートチャート #

あなたは最終的に、すべての農産物を見て回る機会を得ました。
チョコレート、シナモン、ミント、唐辛子、ナツメグ、バニラ...
妖精たちがこれらの植物を栽培しているのは
**ホットチョコレート**を作るために違いありません！
これに気付いたとき、遠くから会話が聞こえてきました。
あなたが調査に行くと、
間に合わせの地下調理場または実験室のように
見える場所に二人の妖精がいるのを見つけました。

妖精たちは究極のホットチョコレートレシピを考え出そうとしています。
彼らは各レシピの品質**スコア**（0～9）を記録するスコアボードを維持しています。

ふたつのレシピだけがボードに上がります。（一対一で対決するということか？）
第1のレシピは3点、第2のレシピは7点をとりました。

2人の妖精はそれぞれ**現在のレシピ**を持っています。
一人目の妖精は第1のレシピで始め、
二人目の妖精は第2のレシピで始めます。

新しいレシピを作成するために、
二人の妖精は現在のレシピを組み合わせます。

これにより、現在のレシピのスコアの合計の数字
から新しいレシピが作成されます。
現在のレシピがそのスコア3と7のもので、
その合計は10、よって
スコア1のものとスコア0のものの
ふたつの新しいレシピが作成されます。
もし現在のレシピの得点が2と3であれば、
その合計の5は数字ひとつだけからなるので、
スコア5のレシピ1つだけが作られます。

新しいレシピは作成された順にスコアボードの最後に追加されます。
だから、最初のラウンドの後、スコアボードは`3, 7, 1, 0`となります。

すべての新しいレシピがスコアボードに追加された後、
妖精はそれぞれ現在のレシピを新たに選択します。
これを行うために、
**現在のレシピのスコアに1を加えた数**だけ
スコアボードの次に進めます。
したがって、最初のラウンド後、
第1の妖精は$1 + 3 = 4$だけ前に移動し、
第2の妖精は$1 + 7 = 8$だけ前に移動します。
レシピが足りなくなると、スコアボードの先頭にループバックします。
この最初のラウンドの後、
両方の妖精は偶然にも回り回って
彼らが最初に持っていたのと同じレシピに到着する。
しかし一般的に、彼らは異なるレシピに移動します。

第1の妖精を丸括弧で、第2の妖精を角括弧で囲み、
彼らがこのプロセスを続ける様子を示します。

~~~
(3)[7]
(3)[7] 1  0
 3  7  1 [0](1) 0
 3  7  1  0 [1] 0 (1)
(3) 7  1  0  1  0 [1] 2
 3  7  1  0 (1) 0  1  2 [4]
 3  7  1 [0] 1  0 (1) 2  4  5
 3  7  1  0 [1] 0  1  2 (4) 5  1
 3 (7) 1  0  1  0 [1] 2  4  5  1  5
 3  7  1  0  1  0  1  2 [4](5) 1  5  8
 3 (7) 1  0  1  0  1  2  4  5  1  5  8 [9]
 3  7  1  0  1  0  1 [2] 4 (5) 1  5  8  9  1  6
 3  7  1  0  1  0  1  2  4  5 [1] 5  8  9  1 (6) 7
 3  7  1  0 (1) 0  1  2  4  5  1  5 [8] 9  1  6  7  7
 3  7 [1] 0  1  0 (1) 2  4  5  1  5  8  9  1  6  7  7  9
 3  7  1  0 [1] 0  1  2 (4) 5  1  5  8  9  1  6  7  7  9  2
~~~

妖精は、いくつかのレシピ（あなたのパズル入力）(?)を作ることで
彼らのスキルが向上すると思います。
しかし、それは時間がかかります。
それ以降の**10個のレシピのスコア**を特定することによって、
これをかなりスピードアップすることができます。例えば：

- もしも妖精が、9個のレシピを作った後に向上すると考えるならば、
スコアボード上の最初の9レシピの**後**の10個のレシピのスコアが
`5158916779`です。(???)
（図の最後の行はそうなっています。）(強調表示できない)
- 5レシピの後、次の10のスコアは`0124515891`です。
- 18レシピの後、次の10のスコアは`9251071085`です。
- 2018レシピの後、次の10のスコアは`5941429882`です。

**あなたのパズル入力のレシピの数の直後の10レシピのスコアは何ですか？**

あなたのパズル入力は793031です。

(3,7で始まるのは確定なのか。)

# パート2 #

それが判明したので、あなたは妖精の計画を逆に考えます。(?)
彼らが本当に知りたいことは、
スコアボードで
スコアがあなたのパズル入力の数字である最初のレシピ列の
左に表示されるレシピの数です。

- `51589` は9レシピの後に最初に表示されます。
- `01245` は5レシピの後に最初に表示されます。
- `92510` は18レシピの後に最初に表示されます。
- `59414` は2018レシピの後に最初に表示されます。

**スコアボードで
あなたのパズル入力のスコア列の左側に
表示されるレシピはいくつですか？**