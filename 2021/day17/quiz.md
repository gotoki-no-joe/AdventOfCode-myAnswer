# 17日目：トリックショット #

あなたはついに妖精からののメッセージを解読しました。
メッセージは「やっほー」だけでした。
そりの鍵を探し続けます。

あなたの眼前に広がるものは、大きな海溝に違いありません。
鍵はそこに落ちてしまったのかも？
調査するために探査装置を送り込むのがよいでしょう。

潜水艦の探査装置射出機は、X軸（前方）およびY軸（上向き、または負の場合は下向き）方向に任意の整数速度で探査装置を発射できます。
初速度を(x,y)で表して、たとえば、
初速度(0,10)は探査装置を真上に発射しますが、
初速度(10,-1)はわずかに下向きの角度で探査装置を前方に発射します。

探査装置の(x,y)位置は(0,0)から始まります。
次に、**ステップ**で移動することにより、何らかの軌道をたどります。
各ステップで、これらの変更は次の順序で発生します。

- 探査装置のX座標は、そのX軸速度だけ増加します。
- 探査装置のY座標は、そのY軸速度だけ増加します。
- 水の抵抗により、探査装置のX軸速度は値0に向かって1だけ変化します。
つまり、0より大きい場合は1減少し、0より小さい場合は1増加し、すでに0の場合は変化しません。
- 重力により、探査装置のY軸速度は1だけ減少します。

探査装置を海溝にうまく入れるには、
何ステップかの後に**目標領域**内に入る軌道に探査装置を乗せる必要があります。

潜水艦のコンピュータはすでにこの目標領域（パズル入力）を算出しています。
例えば：

```
target area: x=20..30, y=-10..-5
```

この目標領域は、
何ステップかの後に探査装置のX座標が20以上30以下、**かつ**Y座標が-10以上-5以下になるような、
初速度(x,y)を求める必要があることを意味しています。

この目標領域が与えられた場合、
何ステップかの後に探査装置が目標領域内に入る初速度のひとつは(7,2)です。

```
.............#....#............
.......#..............#........
...............................
S........................#.....
...............................
...............................
...........................#...
...............................
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTT#TT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
```

この図で `S` は、は探査装置の初期位置 (0,0) です。
X座標は右に増加し、Y座標は上に増加します。
右下には、目標領域内の位置 `T` で表示されています。
各ステップの後（目標領域に到達するまで）、探査装置の位置は `#` で記されています。
（右下の `#` は、探査装置が到達する位置でかつ目標領域内の位置の両方です。）

何ステップかの後に探査装置が目標領域内に入る別の初速度は(6,3)です。

```
...............#..#............
...........#........#..........
...............................
......#..............#.........
...............................
...............................
S....................#.........
...............................
...............................
...............................
.....................#.........
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................T#TTTTTTTTT
....................TTTTTTTTTTT
```

他には (9,0)：

```
S........#.....................
.................#.............
...............................
........................#......
...............................
....................TTTTTTTTTTT
....................TTTTTTTTTT#
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
....................TTTTTTTTTTT
```

どれだけステップしても探査装置が目標領域内に**入らない**初速度はのひとつは(17,-4)です。

```
S..............................................................
...............................................................
...............................................................
...............................................................
.................#.............................................
....................TTTTTTTTTTT................................
....................TTTTTTTTTTT................................
....................TTTTTTTTTTT................................
....................TTTTTTTTTTT................................
....................TTTTTTTTTTT..#.............................
....................TTTTTTTTTTT................................
...............................................................
...............................................................
...............................................................
...............................................................
................................................#..............
...............................................................
...............................................................
...............................................................
...............................................................
...............................................................
...............................................................
..............................................................#
```

探査装置は目標領域を通過しているように見えますが、
どのステップの後もその範囲内にはありません。
代わりに、右下に進み続けます。
最初の数ステップのみが示されています。

最先端科学の探査装置を超クールな射出機から発射するなら、**カッコよく決めましょう**。
目標領域に入れるようにして、探査装置をどこまで高く飛ばせますか？

上の例では、初速度(6,9)を使用するのが最善の方法で、探査装置はY座標が最大45に到達します。
（Y軸初速度をこれより高くすると、探査装置は目標領域を完全に飛び越えます。）

探査装置が最高のY座標に達し、かつ何ステップか後に最終的に目標領域内に入るような初速度を見つけてください。
**この軌道上で到達する最高のY座標はいくつですか？**

# パート2 #

たぶん、派手なトリックショットは最善のアイデアではありません。
結局のところ、探査装置は1つしかないので、失敗はできません。

探査装置を発射するときの最良の選択肢を選ぶには、
何ステップか後に最終的に探査装置が目標領域内に入るような初速度を**全て**見つける必要があります。

上記の例では、この基準を満たす112のさまざまな初速度値があります。

```
23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
8,-2    27,-8   30,-5   24,-7
```

**何ステップか後に探査装置が目標領域内に入るような異なる初速度はいくつありますか？**
