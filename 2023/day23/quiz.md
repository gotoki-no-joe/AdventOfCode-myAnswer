# 23日目: 長い散歩

妖精たちは水のろ過作業を再開します！
きれいな水が島之島の端から流れ落ち始めます。

彼らは、島之島の端を越える手助けも申し出てくれます！
この信じられないほど長いロープの片端をしっかりと握っていれば、
あなたが作った巨大な滝から安全な距離まで降ろしてくれます。

雪之島にたどり着くと、水が地面まで届いていないことに気づきます。それは空気自体に吸収されています。
湿気が雪を生産するレベルに達するまで、ようやく少しの休憩ができそうです。
雪之島は雪がなくてもかなり風光明媚です。散歩に出かけてみませんか？

近くのハイキングコースの地図（あなたのパズルの入力）があります。
地図には道 (`.`)、森 (`#`)、急斜面 (`^`,`>`,`v`,`<`) が示されています。

例えば：

```
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
```

あなたは現在、最上段の一本道のマスにいます。
あなたの目標は、最下段の一本道のマスに到達することです。
滝からの霧のため、斜面はおそらくかなり滑りやすいでしょう。
斜面マスに足を踏み入れると、次のステップは必ず下り（矢印の指す方向）でなければなりません。
できるだけ美しいハイキングを楽しむために、同じマスに二度入らないでください。
あなたが取れる最も長いハイキングはどんなですか？

上記の例では、最も長いハイキングは `O` で示され、出発地点は `S` で示されています。

```
#S#####################
#OOOOOOO#########...###
#######O#########.#.###
###OOOOO#OOO>.###.#.###
###O#####O#O#.###.#.###
###OOOOO#O#O#.....#...#
###v###O#O#O#########.#
###...#O#O#OOOOOOO#...#
#####.#O#O#######O#.###
#.....#O#O#OOOOOOO#...#
#.#####O#O#O#########v#
#.#...#OOO#OOO###OOOOO#
#.#.#v#######O###O###O#
#...#.>.#...>OOO#O###O#
#####v#.#.###v#O#O###O#
#.....#...#...#O#O#OOO#
#.#########.###O#O#O###
#...###...#...#OOO#O###
###.###.#.###v#####O###
#...#...#.#.>.>.#.>O###
#.###.###.#.###.#.#O###
#.....###...###...#OOO#
#####################O#
```

このハイキングは94ステップあります。
（他に選べたハイキングはそれぞれ90, 86, 82, 82, 74ステップの長さでした。）

地図に示されたハイキングコースを辿って、最も長いハイキングを見つけてください。
最も長いハイキングは何ステップの長さですか？

# パート2

登山口に到着すると、地面が思ったほど滑りやすくないことに気づきます。
急な斜面を登るのに問題はありません。

今回は、全ての斜面を普通の道 (`.`) のように扱ってください。
できるだけ景色の良いハイキングを楽しむために、同じタイルに二度入らないでください。
あなたが取れる最も長いハイキングはどれくらいですか？

上の例では、最も長いハイキングが154ステップに増えます：

```
#S#####################
#OOOOOOO#########OOO###
#######O#########O#O###
###OOOOO#.>OOO###O#O###
###O#####.#O#O###O#O###
###O>...#.#O#OOOOO#OOO#
###O###.#.#O#########O#
###OOO#.#.#OOOOOOO#OOO#
#####O#.#.#######O#O###
#OOOOO#.#.#OOOOOOO#OOO#
#O#####.#.#O#########O#
#O#OOO#...#OOO###...>O#
#O#O#O#######O###.###O#
#OOO#O>.#...>O>.#.###O#
#####O#.#.###O#.#.###O#
#OOOOO#...#OOO#.#.#OOO#
#O#########O###.#.#O###
#OOO###OOO#OOO#...#O###
###O###O#O###O#####O###
#OOO#OOO#O#OOO>.#.>O###
#O###O###O#O###.#.#O###
#OOOOO###OOO###...#OOO#
#####################O#
```

地図に記載されている、予想外に乾燥したハイキングコースの中で、最も長いハイキングを見つけてください。
最も長いハイキングは何ステップの長さですか？
