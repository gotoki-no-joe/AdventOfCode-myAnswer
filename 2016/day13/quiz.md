# 13日目：曲がりくねりキュービクル迷路 #

この新しい建物の1階に到着すると、そこは前の建物の光沢のあるアトリウムとは違ってはるかに居心地の悪い環境でした。
代わりに、あなたはねじれた小さなキュービクルの迷路の中にいます。

この領域のすべての場所は、負でない整数の対 (x,y) でアドレス指定されます。
そのような各座標は、壁または何もない空間かのいずれかです。
斜めに移動することはできません。
キュービクルの迷路は (0,0) から始まり**正の** x と y 方向に無限に広がっているようです。
負の値は建物の外の場所を表すため**無効**です。
あなたは (1,1) の小さな待合室にいます。

混沌としているように見えますが、近くの士気向上ポスターが説明しているように、レイアウトは実際には非常に論理的です。
単純な手順を使用して、与えられた特定の座標 (x,y) が壁であるか空きであるかを決定できます。

- $x*x + 3*x + 2*x*y + y + y*y$ を求めます。
- オフィスデザイナーのお気に入りの番号（パズルの入力）を加えます。
- その合計の二進表現を見つけます。その1であるビット数を数えます。
  - 1であるビットの数が**偶数**の場合、そこは空間です。
  - 1であるビットの数が**奇数**の場合、それは壁です。

たとえば、オフィスデザイナーのお気に入りの番号が10、壁を`#`、空間を`.`で表すと、(0,0)を含む建物の隅は次のようになります。

~~~
  0123456789
0 .#.####.##
1 ..#..#...#
2 #....##...
3 ###.#.###.
4 .##..#..#.
5 ..##....#.
6 #...##.###
~~~

ここで、(7,4) に到達したいとします。
あなたが取ることができる最短経路を`O`で示します：

~~~
  0123456789
0 .#.####.##
1 .O#..#...#
2 #OOO.##...
3 ###O#.###.
4 .##OO#OO#.
5 ..##OOO.#.
6 #...##.###
~~~

したがって、あなたの現在の場所(1,1)から開始して (7,4) に到達するには最小で11ステップが必要です。

あなたが (31,39) に到達するために必要な最小のステップ数はいくつですか？

あなたのパズル入力は1352です。

# パート2 #

たかだか50ステップで、開始場所を含む異なる(x,y)座標としていくつの場所に到達できますか？