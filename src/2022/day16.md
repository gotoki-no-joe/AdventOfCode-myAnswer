# 16 日目: 好火山性ゾウ目

（原題 Proboscidea Volcanium
好熱好酸性古細菌の学名 Thermoplasma volcanium に Proboscidea 長鼻目、別名ゾウ目 をはめ込んで
学名っぽくしたゾウ語と思われる。）

センサーによりあなたを遭難信号の発信源に到達しました。
それは、小人があなたに渡したものと同じような、また別の携帯機器です。
ただし、周りに小人はいません。
代わりに、機器を象が取り囲んでいます！
彼らはこのトンネルの中で迷子になり、
その中の1頭が遭難信号をオンにする方法を見つけたに違いありません。

地面が再びうなります。今回はさらに激しいです。ここは一体、何の洞窟なのでしょう？
携帯機器で洞窟をスキャンします。大部分が火成岩、いくらかの火山灰、高圧のガス、マグマで構成されています…
これは単なる洞窟ではありません、火山です！

すぐに、ゾウをここから連れ出してください。
あなたの装置は、火山が噴火するまであと**30分**と推定しているため、来た道を戻る時間はありません。

他の選択肢を探すために洞窟をスキャンした結果、パイプと圧力開放**バルブ**のネットワークを発見しました。
どうしてそんなシステムが火山にあるのかはわかりませんが、文句を言っている時間はありません。
機器は、各バルブを開放した場合の**流量**（1分あたりの圧力）と、
バルブ間を移動するために使用できるトンネルに関する報告（パズル入力）を生成します。

あなたとゾウが現在立っている空間にも、`AA` とラベル付けられたバルブがあります。
1つのバルブを開くのに1分、あるバルブから別のバルブへのトンネルをたどるのに1分かかると見積もりました。
あなたが開放できる最大の圧力はどれだけでしょう？

たとえば、次のスキャン出力があるとします。

```
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
```

すべてのバルブは始めは**閉じて**います。
あなたは最初バルブ`AA`の位置にいますが、それは壊れているか、詰まっているか、何かでしょう。
その流量は`0`であるため、バルブを開けても意味がありません。
しかし、1分かけてバルブ`BB`に移動し、さらに1分かけてバルブを開くことができます。
そうすることで、残りの**`28`分間に`13`の流量で圧力が開放され、
最終的に合計で\\(28 \times 13 = 364\\)の圧力が開放されます。
次の3分めでバルブ`CC`に移動し、4分めでバルブを開き、**`26`分間**流量`2`での追加の圧力開放ができ、
結果としてバルブ`CC`により合計`52`の圧力を開放できます。

このようにしてトンネルを進んでいくと、30分経過するまでに多くのバルブまたは全てのバルブを開けるかもしれません。
ただし、できるだけ多くの圧力を解放する必要があるため、順序だてて行う必要があります。
次のアプローチを考えます：

```
== Minute 1 ==
No valves are open.
You move to valve DD.

== Minute 2 ==
No valves are open.
You open valve DD.

== Minute 3 ==
Valve DD is open, releasing 20 pressure.
You move to valve CC.

== Minute 4 ==
Valve DD is open, releasing 20 pressure.
You move to valve BB.

== Minute 5 ==
Valve DD is open, releasing 20 pressure.
You open valve BB.

== Minute 6 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve AA.

== Minute 7 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve II.

== Minute 8 ==
Valves BB and DD are open, releasing 33 pressure.
You move to valve JJ.

== Minute 9 ==
Valves BB and DD are open, releasing 33 pressure.
You open valve JJ.

== Minute 10 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve II.

== Minute 11 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve AA.

== Minute 12 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve DD.

== Minute 13 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve EE.

== Minute 14 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve FF.

== Minute 15 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve GG.

== Minute 16 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You move to valve HH.

== Minute 17 ==
Valves BB, DD, and JJ are open, releasing 54 pressure.
You open valve HH.

== Minute 18 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve GG.

== Minute 19 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve FF.

== Minute 20 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You move to valve EE.

== Minute 21 ==
Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
You open valve EE.

== Minute 22 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You move to valve DD.

== Minute 23 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You move to valve CC.

== Minute 24 ==
Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
You open valve CC.

== Minute 25 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 26 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 27 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 28 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 29 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.

== Minute 30 ==
Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
```

このアプローチにより、このバルブ配置で30分間で可能な最も多くの圧力`1651`を開放できます。

30分で最大の圧力を開放する手順を探してください。
**あなたが開放できる最大の圧力はどれだけですか？**
