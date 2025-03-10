# 25日目：雪よ降れ

メリークリスマス！
サンタはお天気制御装置を起動しています。
あなたは結局[ホワイトクリスマス](./day1.md)を得られそうです。

お天気制御装置がビービーと鳴りました！
装置のコンソールには、
取扱説明書のコードを入力するように求めるコピー防止メッセージが出ています。
どうやら、あなたがそのコードを与えない限り、それは実行を拒否するようです。
別に問題はありません。あなたはマニュアルからコードを調べるでしょう…

「ホー、ホー、ホー」サンタは大声で熟考します。
「マニュアルを見つけ出せる気がしない。」

製造元のサポート電話番号を調べて、電話をかけます。
良いこともあります - その49番目のスターはそれ自体で稼ぐことはできませんでした。
(???)

「ああ、その機械はかなり古いです！」と彼らは言います。
「そのモデルは6分前にサポートを終了しました。
そして、私たちはすべてのマニュアルをシュレッダーにかけたばかりです。
ただし、コード生成アルゴリズムを見つけることができると思います。」

20分間保留にされた後
（あなたの電話は彼らにとって非常に重要であると何度も思い出させられました）、
ようやくコードシステムの仕組みを覚えているエンジニアが見つかりました。

コードは、左上隅から始まる無限の紙に印刷されています。
コードは対角線で埋められます。
空の最初のマスを1つもつ最初の行から始まり、コードは斜めに上と右に向かって埋められます。
このプロセスは、
[無限の紙が覆われる](https://ja.wikipedia.org/wiki/カントールの対角線論法)
まで繰り返されます。
よって、最初のいくつかのコードは次の順序で埋められます。

~~~
   | 1   2   3   4   5   6
---+---+---+---+---+---+---+
 1 |  1   3   6  10  15  21
 2 |  2   5   9  14  20
 3 |  4   8  13  19
 4 |  7  12  18
 5 | 11  17
 6 | 16
~~~

たとえば、12番目のコードは4行2列に書き込まれます。
15番目のコードは1行5列に書き込まれます。

電話の向こうの声は、続けてコードが実際にどのように生成されるかを説明します。
最初のコードは`20151125`です。
その後、各コードは、前のコードに`252533`を乗算してから、
その値を`33554393`で除算した余りとして生成されます。

したがって、2番目のコード（2行1列にくる）を見つけるには、
前の値`20151125`から始めます。
それに`252533`を掛けて`5088824049625`を得ます。
そしてそれを`33554393`で割ると余りが`31916031`となります。
この余りが2番目のコードです。

「ああ！」と声がします。
「マニュアルにコードのページがあるのを見落としていたようです。読んでみましょう。」
あなたは彼の読み上げる番号を書き留めます。

~~~
   |    1         2         3         4         5         6
---+---------+---------+---------+---------+---------+---------+
 1 | 20151125  18749137  17289845  30943339  10071777  33511524
 2 | 31916031  21629792  16929656   7726640  15514188   4041754
 3 | 16080970   8057251   1601130   7981243  11661866  16474243
 4 | 24592653  32451966  21345942   9380097  10600672  31527494
 5 |    77061  17552253  28094349   6899651   9250759  31663883
 6 | 33071741   6796745  25397450  24659492   1534922  27995004
~~~

「さて、覚えておいて」声は続きます。
「それは最初のいくつかの数字のすべてでさえありません。
例えば、あなたは行6列2の前に来るはずの行7列1の内容がありません。
でもこれだけ判っていれば充分で…
ああ、昼食の時間だよ、さようなら」通話が切断されます。

サンタはオロオロしています。
あなたのパズル入力には、装置のコンソールにあるメッセージが含まれています。
**あなたは装置にどんなコードを与えますか？**

# パート2 #

装置は活気を取り戻し、その後また沈黙します。
ビープ音がします。
「燃料不足」とコンソールに表示されます。
「先へ進むには**50個のスター**が必要です。**スターがひとつ**利用可能です。」

…「スターがひとつ利用可能？」あなたは燃料タンクを点検します。
確かに、唯一のスターがその友人を待って、一番下に座っています。
49個は自分で用意する必要があるようです。

## スターが足らない場合
あなたは装置を起動するのに十分な星がありません。さらに(?)個必要です。

## スターが49揃った場合
あなたはお天気制御装置を起動するのに充分なスターを持っています。

あなたはお天気制御装置に50個のスターを充填します。それは息を吹き返しました！

**雪が降り始めます。**
