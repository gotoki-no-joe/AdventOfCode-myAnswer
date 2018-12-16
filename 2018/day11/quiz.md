# 11日目：時空充電 #

あなたは妖精と彼らのそりが北極に向かって遠ざかり霞んでゆくのを見送った。

実際は、霞んでいるのはあなたの方です。落下感覚が戻ってきました。

腕に取り付けられたデバイスの残燃料警告灯が点灯します。
一度それをタップすると、状況のホログラムが投影されます。
300x300グリッドの燃料セルと現在の出力レベル、そのうちいくつかはマイナスになっています。
時間旅行という文脈でマイナスの出力とはどういう意味なのかよくわかりませんが、
少なくとも良いことではないでしょう。

各燃料セルはX軸（水平方向）とY軸（垂直方向）の両方に
**1～300**の範囲の座標を持ちます。
`X,Y`という表記で、左上のセルは`1,1`、右上のセルは`300,1`です。

インタフェースを用いて、**任意の3x3四方**の燃料セルを選択できます。
目的地に到達する機会を増やすために、
**最大の総出力**を持つ3x3四方を選ぶことにします。

指定した燃料セルの出力レベルは、以下の手順により得られる。

- 燃料セルの**ラックID**を得る。これはX座標に10を加えたものである。
- 出力レベルは**ラックID**×**Y座標**で始まる。
- **グリッドのシリアル番号**（あなたのパズル入力）の値で出力レベルを増加させる。
- 出力レベルに**ラックID**を掛けた値を設定します。
- 出力レベルの**100の位**の数字だけを保持する。
よって`12345`は`3`となる。100に満たない値は`0`となる。
- 出力レベルから5を引く。

たとえば、グリッド中の位置`3,5`シリアル番号8のの燃料セルの出力レベルを求めるには、

- ラックIDは$3 + 10 = 13$です。
- 出力レベルは$13 * 5 = 65$で始まります。
- シリアル番号を足すと$65 + 8 = 73$が生成されます。
- ラックIDを乗算すると$73 * 13 = 949$が生成されます。
- 949の百の桁の数字は9です。
- 5を減算すると$9 - 5 = 4$になります。

よって、この燃料セルの出力レベルは4です。

さらにパワーレベルの例をいくつか示します。

- 座標122,79 シリアル番号57の燃料セルの出力レベルは-5
- 座標217,196 シリアル番号39の燃料セルの出力レベルは0
- 座標101,153 シリアル番号71の燃料セルの出力レベルは4

あなたの目標は、最大の総出力を持つ3x3四方を見つけることです。
四方300×300グリッド内に完全に入っている必要があります。
左上の燃料セルのX,Y座標を使用してこの四方を特定します。
例えば：

グリッドのシリアル番号が18のとき、
最大出力の3x3四方の左上隅は33,45（合計出力29）です。
これらの燃料セルはこの5x5領域の中央に現れます。

~~~
-2  -4   4   4   4
-4   4   4   4  -5
 4   3   3   4  -4
 1   1   2   4  -3
-1   0   2  -5  -2
~~~

グリッドのシリアル番号が42のとき、
最大の3x3四方の左上は21,61（合計出力30）です。
これらはこの領域の真中にあります。

~~~
-3   4   2   2   2
-4   4   3   3   4
-5   3   3   4  -4
 4   3   3   4  -3
 3   3   3  -5  -1
~~~

**総出力が最大の3x3四方の左上の燃料セルの座標は？**

あなたのパズル入力は9005です。

# パート2 #

あなたはデバイスの側面にダイヤルを見つけた。
これで3x3四方だけでなく、**任意の大きさ**の正方形を選択できるようです。
1x1から300x300までの大きさに対応しています。

これを理解した上で、
**最大の総出力を持つ任意の大きさの正方形**を見つける必要があります。
この正方形を、左上の座標の後の3番目のパラメータとして大きさを指定します。
左上の角が3,5で9x9の正方形は3,5,9で識別されます。

例えば：

- グリッドシリアル番号が18のとき、最大の正方形（総出力113）は16x16で
左上隅は90,269なので、は識別子は90,269,16となります。
- グリッドシリアル番号の42のとき、最大の正方形（総出力119）は12x12で
左上隅は232,251なので、識別子は232,251,12となります。

**最大の総出力を持つ正方形のX,Y,size識別子は何ですか？**