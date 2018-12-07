# 2日目：破損チェックサム #

あなたが歩いてドアを通り抜けると、
光るヒューマノイドの形があなたの方向に叫ぶ。
(それどっち？)
「そこのお前！
お前はアイドル状態にあるように見える。
このスプレッドシートの破損を修復する手伝いに来てください。
もう1ミリ秒時間が経つと、
砂時計カーソルを表示する必要があります！」

スプレッドシートは、明らかに乱数の列で構成されています。
回復プロセスが適切な軌道上にあることを確認するために、
スプレッドシートの**チェックサム**をあなたが計算する必要があります。
各行について、最大値と最小値の差を求めます。
チェックサムはこれらの差の合計です。

たとえば、次のスプレッドシートがあるとします。

~~~
5 1 9 5
7 5 3
2 4 6 8
~~~

- 1行目の最大値及び最小値は9と1で、その差は8です。
- 2行目の最大値及び最小値は7と3で、その差は4です。
- 3行目の差は6です。

この例では、スプレッドシートのチェックサムは
$8 + 4 + 6 = 18$となります。

あなたのパズル入力のスプレッドシートの**チェックサムは何ですか？**

# パート2 #

「やったね！
結局のところ、私たちは正しい軌道に乗っているように見えます。
あなたの頑張りに**スター**を授けよう。
しかし、プログラムは少し当惑しているようです。
…プログラムが**当惑する**ことなんてあるの？

「私たちが見ているものに基づいて、
ユーザーが必要とするものは
スプレッドシート中の**割り切れる値**の情報だけである
ように見える。
残念ながら、私たちは誰もその種の計算能力を装備していません。
私たちのほとんどはビット演算が専門です。」

目標は、
各行において
片方がもう一方を割り切る、
すなわち除算の結果が整数である
ような二つの数の唯一の対を発見すること
であるということらしい。
彼らは、
あなたが各行でそれらの数字を見つけて、
除算をして、各行の結果を足し合わせることを望んでいる。

たとえば、次のスプレッドシートがあるとします。

~~~
5 9 2 8
9 4 7 3
3 8 6 5
~~~

- 1行目では、唯一割り切れる2つの数の対は8と2です。
割った結果は4です。
- 2行目では、2つの数は9と3です。結果は3です。
- 3行目では、結果は2です。

この例では、結果の合計は$4 + 3 + 2 = 9$となります。

あなたのパズル入力での**各行の結果の合計**はいくつですか？