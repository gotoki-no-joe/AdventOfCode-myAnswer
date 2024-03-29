# 13日目：透ける折り紙 #

あなたは洞窟の別の火山活動の活発な部分に到達します。
どの洞窟が暑すぎて安全に入ることができないかを前もって知ることができるように、
何らかの熱画像を撮ることができれば素晴らしいでしょう。

幸いなことに、潜水艦には赤外線カメラが装備されているようです！
それをアクティブにすると、次のように歓迎メッセージが表示されました。

> 購入ありがとうございます！
> この赤外線式サーマルカメラシステムを有効化するためには、
> マニュアルの1ページにあるコードを入力してください。

どうやら、妖精はこの機能を使ったことがないようです。
驚いたことに、あなたはなんとかマニュアルを見つけることができました。
それを開くと、1ページ目が抜け落ちました。
それは大きな透き通った紙です！
透明な紙はランダムに点が記されており、
折りたたむ方法（パズルの入力）の説明が含まれています。
例えば：

```
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
```
> fold along y=7 : y=7に沿って折る

前半は、透明な紙に記された点のリストです。
`0,0` は左上の座標を表します。
最初の値 `x` は右に向かって増加します。
2番目の値 `y` は下向きに増加します。
したがって、座標 `3,0` は `0,0` の右側にあり、
座標 `0,7` は `0,0` の下にあります。
この例の座標は、次のパターンを形成します。
ここで `#` は紙の上の点で、`.` は点のない空の位置です。

```
...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
```

次に、折り方の指示のリストがあります。
それぞれの指示は、透明な紙の上の線を表しており、
`y=...` という水平の線のときは**上**に、
`x=...` という垂直の線のときは**左**に折れという指示です。
この例では、最初の折り方の指示は `fold along y=7` です。
これは、yが7（下では `-` で示している）のすべての位置によって形成される線を指定します。

```
...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
```

これは横線なので、下半分を**上**に折ります。
折り畳みが完了した後、一部の点が重なってしまう可能性がありますが、
折り線がちょうど点のところにくることはありません。
この折り畳みを行った結果は次のようになります。

```
#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
```

現在、点は17個だけ見えます。

たとえば、透明な紙を折り畳む前の左下隅にある2つの点に注意してください。
折り畳みが完了すると、それらの点が左上隅（`0,0`と`0,1`）に現れます。
紙は透明なので、折り畳んだ後のこの二つの点のすぐ下（座標`0,3`）の点は、
透明な紙を通して見ることができることから、見え続けています。

また、一部の点が重なってしまう可能性があることにも注意してください。
この場合、点は重なり合ってひとつの点になります。

ふたつめの折り方の指示は `fold along x=5` で、これは以下の図の線を意味します。

```
#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....
```

これは垂直線なので、**左**に折ります：

```
#####
#...#
#...#
#...#
#####
.....
.....
```

指示に従うことで正方形ができました！

透明な紙はかなり大きいので、今のところ、最初の折り目を完成させることに集中してください。
上記の例の最初の折り畳みの後、17個の点が見えます。
折り畳みが完了した後に重なり合う点は、単一の点として数えます。

**あなたの透明な紙で最初の折り方の指示を完了した後、点はいくつ見えますか？**

# パート2 #

指示に従って透明紙の折り畳みをやり遂げます。
マニュアルによると、コードは必ず**大文字8文字**です。

**赤外線サーマルカメラシステムを有効化するためにどんなコードを使用しますか？**
