# 5日目：ツイスターのトランポリン迷路、全員 #

緊急割り込みがCPUから到着しました。
それはジャンプ命令の迷路に閉じ込められ、
出口を見つけるために余裕のCPUサイクルを持つ
プログラムの助けを必要としています。

メッセージには、それそれのジャンプのオフセットのリストが含まれています。
ジャンプは相対的です。
`-1`は前の命令に移動し、`2`は次の命令をスキップします。
リストの最初の命令から始めます。
目標は、リストの**外**に出るまでジャンプを伝っていくことです。

さらに、これらの命令は少し奇妙です。
各ジャンプの後に、その命令のオフセットが1増加する。
つまり、もしあなたがオフセット3に遭遇したならば、
あなたは3命令先に進みますが、
それは次に遭遇するときのために4に変化します。

たとえば、次のジャンプオフセットのリストを考えてみましょう。

~~~
0
3
0
1
-3
~~~

正へのジャンプ（「前方」）は下方に移動します。
負のジャンプは上に移動します。
この例の読みやすさのために、
これらのオフセット値を1行で書き、現在の命令をカッコで囲んで示します。
出口が見つかるまでに、次の手順が実行されます。

- `(0) 3  0  1  -3` ステップを進める**前**の状態
- `(1) 3  0  1  -3` オフセットでジャンプする0（つまり、全くジャンプしない）。
幸運にもこの命令はここで1にインクリメントされる。
- ` 2 (3) 0  1  -3` 今修正した命令により前進する。
最初の命令がもう一度インクリメントされ2になります。
- ` 2  4  0  1 (-3)` 最後まで跳び、後に4を残す。
- ` 2 (4) 0  1  -2`  さっきいた場所に戻ります。-3を-2まで増やします。
- ` 2  5  0  1  -2` 前方に飛んで迷路を抜けます。

この例では、5ステップで出口に到達しています。

出口に行くには**何ステップ**かかりますか？

# パート2 #

今度は、ジャンプは奇妙なものになります。
ジャンプするごとに、オフセットが**3以上**だったら、
代わりにそれを1減らしてください。
それ以外の場合は、以前と同じように1増やしてください。

上記の例でこのルールを使用すると、
プロセスは10ステップを要し、
出口を見つけた後のオフセット値は
`2 3 2 3 -1`のままになります。

今回は出口に到達するまでに**何ステップ**かかりますか？