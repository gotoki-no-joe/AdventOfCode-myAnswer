# 21日目: 歩数計

あなたは、全額負担で無人島へ旅行する他の人を送り届ける飛行船を、なんとか捕まえることができました。庭師と彼の広大な農場の近くまで降ろしてくれるので便利です。

「また砂が流れてきたね！素晴らしい仕事だ！あとはスノー島の水をろ過するのに十分な砂が手に入るまで待つだけ。すぐにまた雪が降るよ。」

あなたが待っている間、庭師と協力しているエルフの一人が、あなたが問題解決に優れていることを聞き、助けを求めてきました。彼はその日の歩数を増やす必要があるため、残りの歩数で正確にどの庭の区画に到達できるかを64知りたいと考えています。

S彼は、開始位置 ( )、庭の区画 ( .)、および岩 ( )の最新の地図 (パズルの入力) を提供します#。例えば：

...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
エルフは開始位置 ( S) から開始します。これも庭の区画としてカウントされます。その後、北、南、東、西に 1 歩ずつ進むことができますが、それは庭の区画であるタイルの上にのみです。これにより、彼は次のマークが付いたタイルのいずれかに到達できるようになりますO。

...........
.....###.#.
.###.##..#.
..#.#...#..
....#O#....
.##.OS####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
そして、彼は第二歩を踏み出します。この時点で、彼は とマークされたタイルのいずれかOにいる可能性があるため、2 番目のステップにより、最初のステップの後に到達できたタイルから 1 歩北、南、東、または西にある庭の区画に到達できるようになります。

...........
.....###.#.
.###.##..#.
..#.#O..#..
....#.#....
.##O.O####.
.##.O#...#.
.......##..
.##.#.####.
.##..##.##.
...........
2 歩進むと、開始位置を含む、上でマークされたタイルのいずれかに到達できますO(北から南に移動するか、西から東に移動することによって)。

3 番目のステップを 1 つ行うだけで、さらに多くの可能性が広がります。

...........
.....###.#.
.###.##..#.
..#.#.O.#..
...O#O#....
.##.OS####.
.##O.#...#.
....O..##..
.##.#.####.
.##..##.##.
...........
彼はその日の歩数が尽きるまでこの作業を続けるだろう。合計の手順を踏むと6、次のマークが付けられた庭の区画のいずれかに到達できますO。

...........
.....###.#.
.###.##.O#.
.O#O#O.O#..
O.O.#.#.O..
.##O.O####.
.##.O#O..#.
.O.O.O.##..
.##.#.####.
.##O.##.##.
...........
この例では、エルフの目標が6今日より正確に歩数を増やすことであった場合、彼はその歩数を使用して16庭の区画のいずれかに到達できます。

しかし、エルフは実際に今日歩数を取得する必要があり64、彼があなたに手渡した地図はサンプルの地図よりもはるかに大きいです。

S地図上にマークされた庭園区画から出発して、エルフは正確な64ステップでいくつの庭園区画に到達できるでしょうか?

まず、パズルの入力を取得します。

答え：
 

［シェア］もできます】このパズル。

- - パート2 - -
エルフは自分の間違いに気づくまで、あなたの答えに混乱しているようです。彼は歩数計ではなく、完全平方と完全立方体の両方であるお気に入りの数字のリストを読んでいたのです。

彼が今日取得する必要がある実際の歩数は、ちょうど です26501365。

彼はまた、庭園の区画や岩が、地図があらゆる方向に無限に繰り返されるように設定されていることも指摘しています。

したがって、上記のサンプル マップの端からさらに 1 つのマップ幅またはマップ高さを探すと、それが繰り返されていることがわかります。

.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##...####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................
.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##..S####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................
.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##...####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................
これは、不可解なほど無限に広がる農場のレイアウトの、3 マップ×3 マップの小さなスライスにすぎません。見渡す限り、庭の区画と岩が繰り返されます。ただし、エルフは依然として とマークされた 1 つの中央のタイルから始まりますS。繰り返されるタイルはすべて、S通常の庭の区画 ( ) に置き換えられます.。

この新しい無限バージョンのマップ例で、さまざまな歩数で到達可能な庭の区画の数を次に示します。

正確な6歩数で、彼はまだ16庭の区画に到達することができます。
正確な10手順で、彼はどの50庭の区画にも到達できます。
正確な手順で、彼は庭の区画50に到達できます。1594
正確な手順で、彼は庭の区画100に到達できます。6536
正確な手順で、彼は庭の区画500に到達できます。167004
正確な手順で、彼は庭の区画1000に到達できます。668697
正確な手順で、彼は庭の区画5000に到達できます。16733044
ただし、エルフに必要な歩数はさらに多くなります。あなたの無限の地図上にマークされた庭園区画から出発してS、エルフは正確な26501365ステップでいくつの庭園区画に到達できるでしょうか?
