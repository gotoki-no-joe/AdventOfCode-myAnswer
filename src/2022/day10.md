# 10日目: ブラウン管 (CRT)

ロープを避け、川に飛び込み、岸まで泳ぎます。

小人は上流で彼らと再会することについて何かを叫んでいますが、
川の流れが騒がしすぎて彼らが何を言っているのか正確にはわかりません。
彼らは橋を渡り終え、視界から消えます。

小人があなたの携帯端末の通信システムを機能させることを優先した理由こそ、
このような状況に違いありません。
リュックから取り出してみると、画面の大きなひび割れからゆっくりと水が流れ出ていて、
すぐにはあまり役に立たないことがわかります。

**そうでもない！**
つまり、あなたは装置のビデオシステムの代替品を設計できるからです！
それは、正確な**クロック回路**で駆動される、
ある種の[ブラウン管](https://ja.wikipedia.org/wiki/%E3%83%96%E3%83%A9%E3%82%A6%E3%83%B3%E7%AE%A1)画面と
簡単なCPUのようです。
クロック回路は一定の速さで時を刻みます。それぞれの刻みを**サイクル**と呼びます。

まず、CPUから送信されている信号を理解することから始めます。
CPUには単一のレジスタ`X`があります。これは値`1`で始まります。
CPUは次の2つの命令のみをサポートします。

- `addx V` 完了するまでに**2サイクル**かかります。
2サイクル**後**、`X`レジスタは値`V`だけ増加します。
（`V`は負の値になることもあります。）
- `noop` 完了するまでに**1サイクル**かかります。それ以外の効果はありません。

CPUは、プログラム（パズル入力）にあるこれらの命令を使用して、
どういうわけか、何を描画するかを画面に伝えます。

次の小さなプログラムを考えましょう：

```
noop
addx 3
addx -5
```

このプログラムの実行は次のように進行します。

- 最初のサイクルの開始時に、`noop`命令の実行が開始されます。
最初のサイクルでは、`X`は`1`です。
最初のサイクルの後、`noop`命令は何もせずに実行を終了します。
- 2番目のサイクルの開始時に、`addx 3`命令の実行が開始されます。
2番目のサイクル中は、`X`はまだ`1`です。
- 3番目のサイクルでは、`X`はまだ`1`です。
3番目のサイクルの後、`addx 3`命令は実行を終了し、`X`は`4`に設定されます。
- 4番目のサイクルの開始時に、`addx -5`命令の実行が開始されます。
4番目のサイクルの間、`X`はまだ`4`です。
- 5番目のサイクルの間、`X`はまだ`4`です。
5番目のサイクルの後、`addx -5`命令は実行を終了し、`X`は`-1`に設定されます。

実行中にレジスタ`X`の値を観察することで、何かを学ぶことができるかもしれません。
ここでは、20番目のサイクルとその後40サイクルごと
（つまり、20番目、60番目、100番目、140番目、180番目、220番目のサイクル）の信号強度
（サイクル数に`X`レジスタの値を乗じた値）を考えてみましょう。

例えば、次のもう少し大きいプログラムを考えてみましょう：

```
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
```

興味のある信号強度は、次のように求められます：

- 20番目のサイクルでは、 レジスタ`X`の値は`21`であるため、信号強度は\\(20 \times 21 = 420\\)です。
(20番目のサイクルは二つめの`addx -1`の途中で起き、
このときのレジスタ`X`の値は、開始値の`1`と、この位置までの全ての`addx`命令の値を足し合わせた値となります。
\\(1 + 15 - 11 + 6 - 3 + 5 - 1 - 8 + 13 + 4 = 21\\)
- 60番目のサイクルでは、レジスタ`X`の値は`19`であるため、信号強度は\\(60 \times 19 = 1140\\)です。
- 100番目のサイクルでは、レジスタ`X`の値は`18`であるため、信号強度は\\(100 \times 18 = 1800\\)です。
- 140番目のサイクルでは、レジスタ`X`の値は`21`であるため、信号強度は\\(140 \times 21 = 2940\\)です。
- 180番目のサイクルでは、レジスタ`X`の値は`16`であるため、信号強度は\\(180 \times 16 = 2880\\)です。
- 220番目のサイクルでは、レジスタ`X`の値は`18`であるため、信号強度は\\(220 \times 18 = 3960\\)です。

これらの信号強度の合計は**13140**です。

20回目、60回目、100回目、140回目、180回目、220回目のサイクルの信号強度を求めます。
**これらの6つの信号強度の合計はいくつですか？**

<!--
<details><summary>解説</summary><div>

命令列を行ごとに区切って与える。
命令列によって、時間が流れてXの値が変化していく。
次のクロックの開始時のXの値を生成するループを作る。

```haskell
loop :: Int -> [String] -> [Int]
loop x (cs:css) =
  case words cs of
    ["noop"] -> x : loop x css
    ["addx",arg] -> let x1 = x + read arg in x : x1 : loop x1 css
loop x [] = [x]
```

必要な時刻のレジスタ値を抜き出して答えを計算する。

```haskell
body1 fn = do
  ls <- lines <$> readFile fn
  let xs = 0 : loop 1 ls
  print $ sum $ map (\i -> i * xs !! pred i) [20,60..220]

main1 = body1 "input.txt"
```

</div></details>
-->

# パート2

`X`レジスタが[スプライト](https://ja.wikipedia.org/wiki/%E3%82%B9%E3%83%97%E3%83%A9%E3%82%A4%E3%83%88_(%E6%98%A0%E5%83%8F%E6%8A%80%E8%A1%93))の水平位置を制御しているようです。
具体的には、スプライトの幅は3ピクセルで、レジスタ`X`はそのスプライトの**中央**の水平位置を設定します。
（このシステムでは、「垂直位置」のようなものはありません。
CRTが現在描画している場所にスプライトの水平位置がピクセルを置くとき、
それらのピクセルが描画されます。）

あなたはCRTのピクセルを数えます: 幅40ピクセル、高さ6ピクセルです。
このCRT画面は、ピクセルの一番上の行を左から右に描画し、次にその下の行というように描画します。
各行の左端のピクセルは位置`0`にあり、各行の右端のピクセルは位置`39`にあります。

CPUと同様に、CRTはクロック回路と密接に結びついています。
CRTは**各サイクルで1つのピクセルを描画します**。
画面の各ピクセルを`#`で表して、
各行の最初と最後のピクセルが描画されるサイクルは次のとおりです。

```
Cycle   1 -> ######################################## <- Cycle  40
Cycle  41 -> ######################################## <- Cycle  80
Cycle  81 -> ######################################## <- Cycle 120
Cycle 121 -> ######################################## <- Cycle 160
Cycle 161 -> ######################################## <- Cycle 200
Cycle 201 -> ######################################## <- Cycle 240
```

したがって、 CPU命令とCRT描画操作のタイミングを慎重に調整することで、
各ピクセルが描画された瞬間にスプライトが表示されるかどうかを決定できるはずです。
3つのピクセルのうちの1つが現在描画されているピクセルになるようにスプライトが配置された場合、
画面は**点灯している**ピクセル (`#`) を生成します。
そうでない場合、画面はピクセルを**暗いまま** (`.`) にします。

上の大きな例の最初の数ピクセルは、次のように描画されます。

```
Sprite position: ###.....................................

Start cycle   1: begin executing addx 15
During cycle  1: CRT draws pixel in position 0
Current CRT row: #

During cycle  2: CRT draws pixel in position 1
Current CRT row: ##
End of cycle  2: finish executing addx 15 (Register X is now 16)
Sprite position: ...............###......................

Start cycle   3: begin executing addx -11
During cycle  3: CRT draws pixel in position 2
Current CRT row: ##.

During cycle  4: CRT draws pixel in position 3
Current CRT row: ##..
End of cycle  4: finish executing addx -11 (Register X is now 5)
Sprite position: ....###.................................

Start cycle   5: begin executing addx 6
During cycle  5: CRT draws pixel in position 4
Current CRT row: ##..#

During cycle  6: CRT draws pixel in position 5
Current CRT row: ##..##
End of cycle  6: finish executing addx 6 (Register X is now 11)
Sprite position: ..........###...........................

Start cycle   7: begin executing addx -3
During cycle  7: CRT draws pixel in position 6
Current CRT row: ##..##.

During cycle  8: CRT draws pixel in position 7
Current CRT row: ##..##..
End of cycle  8: finish executing addx -3 (Register X is now 8)
Sprite position: .......###..............................

Start cycle   9: begin executing addx 5
During cycle  9: CRT draws pixel in position 8
Current CRT row: ##..##..#

During cycle 10: CRT draws pixel in position 9
Current CRT row: ##..##..##
End of cycle 10: finish executing addx 5 (Register X is now 13)
Sprite position: ............###.........................

Start cycle  11: begin executing addx -1
During cycle 11: CRT draws pixel in position 10
Current CRT row: ##..##..##.

During cycle 12: CRT draws pixel in position 11
Current CRT row: ##..##..##..
End of cycle 12: finish executing addx -1 (Register X is now 12)
Sprite position: ...........###..........................

Start cycle  13: begin executing addx -8
During cycle 13: CRT draws pixel in position 12
Current CRT row: ##..##..##..#

During cycle 14: CRT draws pixel in position 13
Current CRT row: ##..##..##..##
End of cycle 14: finish executing addx -8 (Register X is now 4)
Sprite position: ...###..................................

Start cycle  15: begin executing addx 13
During cycle 15: CRT draws pixel in position 14
Current CRT row: ##..##..##..##.

During cycle 16: CRT draws pixel in position 15
Current CRT row: ##..##..##..##..
End of cycle 16: finish executing addx 13 (Register X is now 17)
Sprite position: ................###.....................

Start cycle  17: begin executing addx 4
During cycle 17: CRT draws pixel in position 16
Current CRT row: ##..##..##..##..#

During cycle 18: CRT draws pixel in position 17
Current CRT row: ##..##..##..##..##
End of cycle 18: finish executing addx 4 (Register X is now 21)
Sprite position: ....................###.................

Start cycle  19: begin executing noop
During cycle 19: CRT draws pixel in position 18
Current CRT row: ##..##..##..##..##.
End of cycle 19: finish executing noop

Start cycle  20: begin executing addx -1
During cycle 20: CRT draws pixel in position 19
Current CRT row: ##..##..##..##..##..

During cycle 21: CRT draws pixel in position 20
Current CRT row: ##..##..##..##..##..#
End of cycle 21: finish executing addx -1 (Register X is now 20)
Sprite position: ...................###..................
```

プログラムを最後まで実行させると、CRTは次の映像を生成します。

```
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
```

あなたのプログラムによって与えられた画像をレンダリングします。
**CRTに表示される8つの大文字は何ですか？**

<!--
<details><summary>解説</summary><div>

時刻 \\(t\\) のスキャン位置が光るかどうかは、時刻 \\(t-1\\) のレジスタの値で定まる。
スキャン位置 \\((t - 1) \bmod 40\\) と
レジスタの値の差が \\(\pm 1\\) に収まっているとき、ピクセルは光る。

```haskell
import Data.List.Split

body2 fn = do
  ls <- lines <$> readFile fn
  let xs = 1 : loop 1 ls
  let ps = zipWith sprite [0..] xs
  mapM_ putStrLn $ chunksOf 40 ps

sprite t x
  | pred x <= tm && tm <= succ x = '#'
  | otherwise = ' '
  where
    tm = mod t 40

main2 = body2 "input.txt"
```

</div></details>
-->
