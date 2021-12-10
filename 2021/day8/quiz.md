# 8日目:7セグメント検索 #

クジラが洞窟の入口に激突して崩落させた瞬間、辛うじてあなたは洞窟の安全地帯に到達しました。
(?barely)
センサーは、この洞窟の別の出口はここよりはるかに深いところにあることを示しているので、押し進むしかありません。

潜水艦が海底洞窟をゆっくりと通過しているとき、潜水艦の4桁の7セグメントディスプレイが誤動作していることにあなたは気付きました。
それらは脱出中に故障したに違いありません。
それらがないといろいろ困ったことになるので、何が悪いのか調べたほうがよいでしょう。

7セグメントディスプレイの各桁は、aからgと名付けられた7つのセグメントをオンオフすることで数字を表示します。

```
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
```

つまり、1を表示するには、セグメントcとfだけをオンにします。残りはオフにします。
7を表示するには、セグメントa, c, f だけをオンにします。

問題は、それぞれのディスプレイについて、セグメントを制御する信号の接続がごちゃ混ぜになっていることです。
潜水艦は依然として、信号aからgへの出力を生成することによって数字を表示しようとしています。
しかし、それらの配線はセグメントに**でたらめに**接続されています。
さらに悪いことに、セグメントへの接続配線は、4桁のディスプレイごとに違う配線をされています！
（ただし、ひとつのディスプレイ**内**のすべての数字は同じ接続を使用します。）

(?ここ言い回しが難しい)
つまり、セグメントbとgに繋がっている信号線だけがオンになっているとわかったとき、
それは**セグメント**bとgをオンにしようとしたことを意味しません。
2つのセグメントを使う数字は1だけなので、
それはセグメントcとfをオンにしようとしたということを意味しています。
この情報だけでは、
bとgのどちらの信号線がcとfのどちらのセグメントに繋ぐべきかを断定できません。
そのためには、より多くの情報を集める必要があります。

それぞれのディスプレイについて、変化する信号をしばらく観察し、
表示された**10個の相異なる信号パターンすべて**を記録してから、
**4桁の出力値**を1つ書き留めました（パズル入力）。
信号パターンを利用すれば、どのパターンがどの数字に対応するかを見つけ出せるはずです。

たとえば、メモのある行に次のようにあったとします。

```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

（画面に収めるために行が折り返されているかもしれませんが、実際のパズル入力では1行です。）

各行は、10個の**相異なる信号パターン**、区切り文字 `|` 、そして最後に**4桁の出力値**で構成されます。
一行の中では、配線とセグメントの接続は一定です。
（ただし、実際の接続が何であるかはわかりません。）
互いに異なる信号パターンは、潜水艦が現在の配線とセグメントの接続を使用して数字を表示しようとしている10の異なる方法に対応しています。
3つのセグメントを使用する唯一の数字が7であるため、
上記の例の中の dab は、7を表示するために信号線 d, a, b がオンにされるとわかります。
4つのセグメントを使用する唯一の数字が4であるため、
eafb は、4を表示するために信号線 e, a, f, b がオンにされるとわかります。

この情報を用いると、どの信号線の組み合わせがそれぞれ10個の数字のどれに対応するかを判断できるはずです。
すると、4桁の出力値を復号できます。
残念ながら、上記の例では、出力値のすべての数字 (cdfeb fcadb cdfeb cdbaf) が
5つのセグメントを使用しており、推測がより困難です。

とりあえず、**簡単な数字に集中します。**
この大きな例を考えてみましょう。

```
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
```

数字 1, 4, 7, 8 は使うセグメント数が独特なので、
どの信号の組み合わせがどの数字に対応するのかを見分けられるはずです。

（各行の `|` より後の部分の）**出力値の数字だけを数える**と、
上記の例では、独特なセグメント数を使用する数字が26回現れています。
（原文では強調表示されています。）

**出力値の中に、1, 4, 7, 8 の数字は何回現れていますか？**

# パート2 #

しばらく推理して、残りの数字も判別できるようになりました。
上記の最初の例をもう一度考えてみましょう。

```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```

注意深い分析の結果、信号線とセグメント間の対応は、次の構成でのみ意味をなします。

```
 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
```

したがって、個々の信号パターンは次の数字に対応します。

- acedgfb: 8
- cdfbe: 5
- gcdfa: 2
- fbcad: 3
- dab: 7
- cefabd: 9
- cdfgeb: 6
- eafb: 4
- cagedb: 0
- ab: 1

すると、出力値の4桁を復号できます。

- cdfeb: 5
- fcadb: 3
- cdfeb: 5
- cdbaf: 3

したがって、この行の出力値は5353です。

上の2つめの大きな例の各行に対してこれと同じ手順を実行すると、各行の出力値を決定できます。

- fdgacbe cefdb cefbgd gcbe: 8394
- fcgedb cgb dgebacf gc: 9781
- cg cg fdcagb cbg: 1197
- efabcd cedba gadfec cb: 9361
- gecf egdcabf bgf bfgea: 4873
- gebdcfa ecba ca fadegcb: 8418
- cefg dcbef fcge gbcadfe: 4548
- ed bcgafe cdgba cbgef: 1625
- gbdfcae bgc cg cgb: 8717
- fgae cfgab fg bagce: 4315

この大きな例ですべての出力値を足し合わせると、結果は 61229 になります。

それぞれの行について、すべての配線とセグメントの接続を決定し、4桁の出力値を復号してください。
**すべての出力値を合計するといくつですか？**
