# 9日目：ビー玉マニア #

ナビゲーションシステムが初期化されるのを待つ間、あなたは妖精エルフと話します。
暇つぶしに、彼らは大好きな**ビー玉**のゲームにあなたを誘います。

妖精は非常に独特なルールに従ってビー玉を**丸く**
順番に並べることによってこのゲームを遊びます。
ビー玉は0から順に1ずつすべてに番号が振られています。

まず、番号0が付けられたビー玉が円の中に置かれます。
この時点では、単一のビー玉だけが含まれていますが、それでも円です。

このビー玉はそれ自体が時計回りに隣でもあり、
それ自体が反時計回りに隣でもあります。
このビー玉を**今のビー玉**と呼びます。

それから、妖精はそれぞれビー玉の円の中に、
**残っているビー玉で最も小さい番号のもの**を
今のビー玉から**時計回り**に1つめと2つめのものとの間に置きます。
（円が充分に大きければ、
これは単に今置かれたビー玉と今のビー玉の間に1つビー玉があることを意味します。）
次に
今置かれたビー玉が今のビー玉になります。

しかし、今置こうとしているビー玉の番号が23の倍数である場合、
**まったく異なる何かが起きます。**
まず、現在のプレイヤーはビー玉をそのまま残して、それを**スコア**に加えます。
また、今のビー玉から7個**反時計回り**のビー玉を円から**除き**、
それも現在のプレイヤーの**得点に加えます**。
除かれたビー玉のすぐ**時計回り**に隣のビー玉が
新しい**今のビー玉**になります。

たとえば、9人のプレイヤーがいるとします。
番号0値のついたビー玉が中央に置かれた後、
角括弧で囲まれた各プレイヤーの番になります。
これらの番のそれぞれの結果は示すようなビー玉の円を生成する
時計回りは右で、結果として生じる現在のビー玉は括弧でくくる。

~~~
[-] (0)
[1]  0 (1)
[2]  0 (2) 1
[3]  0  2  1 (3)
[4]  0 (4) 2  1  3
[5]  0  4  2 (5) 1  3
[6]  0  4  2  5  1 (6) 3
[7]  0  4  2  5  1  6  3 (7)
[8]  0 (8) 4  2  5  1  6  3  7
[9]  0  8  4 (9) 2  5  1  6  3  7
[1]  0  8  4  9  2(10) 5  1  6  3  7
[2]  0  8  4  9  2 10  5(11) 1  6  3  7
[3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
[4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
[5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
[6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
[7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
[8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
[9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
[1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
[2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
[3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
[4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
[5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
[6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
[7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15
~~~

最後のビー玉が使われれた後、
**最高得点のプレイヤー**になることが目標です。
上記の例が25番のビー玉で終了したと仮定すると、
勝者の得点は$23+9=32$です。
（これは、プレーヤー5が23番のビー玉をを保有し
9番のビー玉をを取り除き、
一方
この非常に短いゲームでは他のプレイヤーは
得点を得られなかったからです。）

ここにいくつか例を示します。

- プレイヤー10人、ビー玉1618番までのとき、最高得点は8317
- プレイヤー13人、ビー玉7999番までのとき、最高得点は146373
- プレイヤー17人、ビー玉1104番までのとき、最高得点は2764
- プレイヤー21人、ビー玉6111番までのとき、最高得点は54718
- プレイヤー30人、ビー玉5807番までのとき、最高得点は37305

プレイヤー486人、ビー玉70833番までのとき、
**勝った妖精の得点はいくつですか？**

# パート2 #

好奇心の強い妖精はあなたの素早い答えに心を奪われました。

**ビー玉の番号が100倍だったら勝利点は何点？**