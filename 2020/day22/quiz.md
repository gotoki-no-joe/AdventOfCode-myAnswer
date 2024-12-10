# 22日目: カニ合戦

海を筏で航海していると、数時間のうちに退屈が忍び寄ってきます。
幸いなことに、小さな[宇宙カード](../../2019/day22/quiz.md)のデッキを持ってきていました！
**コンバット**のゲームがしたくなりました。
対戦相手もいます：出発する前にあなたの筏に乗り込んできた小さなカニです。

幸いなことに、カニにルールを教えるのに時間はそんなにかかりません。

ゲームが始まる前に、カードを分けて各プレイヤーが自分のデッキを持つようにします（あなたのパズル入力）。
ゲームは一連の**ラウンド**で構成されます：
両方のプレイヤーが自分のデッキの一番上のカードを引き、
カードの数が大きい方のプレイヤーがラウンドに勝ちます。
勝者が両方のカードを取り、自分のデッキの底に入れます。
このとき大きい方のカードが上に来るようにします。
これにより、あるプレイヤーが全てのカードを持つことになると、
そのプレイヤーが勝ち、ゲームは終了します。

例えば、次のようなデッキの初期状態を考えます：

```
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
```

この配置は、プレイヤー1のデッキに5枚のカードが含まれており、一番上に9、底に1があることを意味します。
プレイヤー2のデッキにも5枚のカードが含まれており、一番上に5、底に10があります。

最初のラウンドは、両方のプレイヤーがデッキの一番上のカード、9と5を引くことから始まります。
プレイヤー1のカードが大きいため、両方のカードはプレイヤー1のデッキの下に移動し、9が5の上に来るようにします。
いずれかのプレイヤーが全てのカードを持つまでに全部で29ラウンドかかります：

```
-- Round 1 --
Player 1's deck: 9, 2, 6, 3, 1
Player 2's deck: 5, 8, 4, 7, 10
Player 1 plays: 9
Player 2 plays: 5
Player 1 wins the round!

-- Round 2 --
Player 1's deck: 2, 6, 3, 1, 9, 5
Player 2's deck: 8, 4, 7, 10
Player 1 plays: 2
Player 2 plays: 8
Player 2 wins the round!

-- Round 3 --
Player 1's deck: 6, 3, 1, 9, 5
Player 2's deck: 4, 7, 10, 8, 2
Player 1 plays: 6
Player 2 plays: 4
Player 1 wins the round!

-- Round 4 --
Player 1's deck: 3, 1, 9, 5, 6, 4
Player 2's deck: 7, 10, 8, 2
Player 1 plays: 3
Player 2 plays: 7
Player 2 wins the round!

-- Round 5 --
Player 1's deck: 1, 9, 5, 6, 4
Player 2's deck: 10, 8, 2, 7, 3
Player 1 plays: 1
Player 2 plays: 10
Player 2 wins the round!

...several more rounds pass...

-- Round 27 --
Player 1's deck: 5, 4, 1
Player 2's deck: 8, 9, 7, 3, 2, 10, 6
Player 1 plays: 5
Player 2 plays: 8
Player 2 wins the round!

-- Round 28 --
Player 1's deck: 4, 1
Player 2's deck: 9, 7, 3, 2, 10, 6, 8, 5
Player 1 plays: 4
Player 2 plays: 9
Player 2 wins the round!

-- Round 29 --
Player 1's deck: 1
Player 2's deck: 7, 3, 2, 10, 6, 8, 5, 9, 4
Player 1 plays: 1
Player 2 plays: 7
Player 2 wins the round!


== Post-game results ==
Player 1's deck: 
Player 2's deck: 3, 2, 10, 6, 8, 5, 9, 4, 7, 1
```

ゲームが終了すると、勝者のスコアを計算できます。
デッキの一番下のカードは、そのカードの値に1を掛けただけの価値があります。
下から二番目のカードは、そのカードの値に2を掛けただけの価値があります。
以下同様に、10枚のカードがある場合、一番上のカードはそのカードの値に10を掛けただけの価値があります。
この例では、勝者のスコアは次の通りです：

```
   3 * 10
+  2 *  9
+ 10 *  8
+  6 *  7
+  8 *  6
+  5 *  5
+  9 *  4
+  4 *  3
+  7 *  2
+  1 *  1
= 306
```

このゲームが終了したとき、勝者のスコアは306です。

あなたが配った2つのデッキを使って、小さなカニとコンバットをします。
勝者のスコアはいくつですか？

# パート2

あなたは小さなカニに負けました！
幸いなことに、カニは再帰が得意ではありません。
筏の船長としての名誉を守るために、小さなカニに**再帰コンバット**のゲームで挑戦します。

再帰コンバットは、最初にカードを2つのデッキに分けることから始まります。
（以前と同じスタートデッキでプレイすることを提案しますーそれが公平です。）
次に、ゲームはいくつかの変更の入った一連の**ラウンド**で構成されます：

- どちらのプレイヤーもカードを配る（訳註：dealでなくてdrawカードを切る、の間違い？）前に、このゲームの以前のラウンドで、
同じ順序で同じカードが同じプレイヤーのデッキにあった場合、
**ゲーム**は即座にプレイヤー1の勝利で終了します。
他のゲームの以前のラウンドは考慮されません。
（これは、誰もが悪いアイデアだと同意する再帰コンバットの無限ゲームを防ぎます。）
- そうでなければ、このラウンドのカードは確かに新しい構成です。
プレイヤーは通常通り、各自のデッキの一番上のカードを引いてラウンドを開始します。
- 両方のプレイヤーが、引いたカードの値と同じかそれ以上の枚数のカードをデッキに残している場合、
ラウンドの勝者は新しい再帰的コンバットのゲームをプレイすることによって決定されます。（下記参照）
- そうでなければ、少なくとも1人のプレイヤーはデッキに再帰するための十分なカードが残っていません。
ラウンドの勝者は、より大きいカードのプレイヤーです。

通常のコンバットと同様に、ラウンドの勝者（サブゲームに勝つことでラウンドを勝ち取った場合でも）は、
ラウンドの開始時に配られた（訳註：drawでは？）2枚のカードを取り、
自分のデッキの一番下に置きます（やはり、勝者のカードが上に来るように）。
サブゲームに勝ったことでラウンドを勝ち取った場合、
勝者のカードは2枚のカードの中で小さい方である可能性があることに注意してください。
ラウンドを勝つことでカードを集めることが、プレイヤーが全てのカードを持つことにつながる場合、
そのプレイヤーが勝利し、ゲームは終了します。

無限ゲーム防止ルールなしでは永遠にループする小さなゲームの例を示します：

```
Player 1:
43
19

Player 2:
2
29
14
```

再帰コンバットのラウンド中、両プレイヤーが自分のデッキに、引いたカードの数と同じかそれ以上の枚数のカードを持っている場合、
そのラウンドの勝者は再帰コンバットのサブゲームに進むことで決定されます。
例えば、プレイヤー1が3のカードを引き、プレイヤー2が7のカードを引いた場合、プレイヤー1が少なくとも3枚、プレイヤー2が少なくとも7枚のカードを残している必要があります。引いた3と7のカードは数えません。

再帰コンバットのサブゲームをプレイするには、
各プレイヤーは自分のデッキから次のカードのコピーを作成して新しいデッキを作ります。
（コピーするカードの枚数は、サブゲームを引き起こすために引いたカードの数字と同じです。）
このサブゲームの間、それを引き起こした元のゲームは保留中で、全く影響を受けません。
サブゲームを形成するためにプレイヤーのデッキからカードが取り除かれることはありません。
（例えば、プレイヤー1が3のカードを引いた場合、サブゲームでの彼のデッキは、
彼のデッキの次の3枚のカードのコピーになります。）

再帰コンバットのゲームプレイの完全な例を示します。
ここで、`Game 1` が今回のプレイの根源(primary)です：

```
=== Game 1 ===

-- Round 1 (Game 1) --
Player 1's deck: 9, 2, 6, 3, 1
Player 2's deck: 5, 8, 4, 7, 10
Player 1 plays: 9
Player 2 plays: 5
Player 1 wins round 1 of game 1!

-- Round 2 (Game 1) --
Player 1's deck: 2, 6, 3, 1, 9, 5
Player 2's deck: 8, 4, 7, 10
Player 1 plays: 2
Player 2 plays: 8
Player 2 wins round 2 of game 1!

-- Round 3 (Game 1) --
Player 1's deck: 6, 3, 1, 9, 5
Player 2's deck: 4, 7, 10, 8, 2
Player 1 plays: 6
Player 2 plays: 4
Player 1 wins round 3 of game 1!

-- Round 4 (Game 1) --
Player 1's deck: 3, 1, 9, 5, 6, 4
Player 2's deck: 7, 10, 8, 2
Player 1 plays: 3
Player 2 plays: 7
Player 2 wins round 4 of game 1!

-- Round 5 (Game 1) --
Player 1's deck: 1, 9, 5, 6, 4
Player 2's deck: 10, 8, 2, 7, 3
Player 1 plays: 1
Player 2 plays: 10
Player 2 wins round 5 of game 1!

-- Round 6 (Game 1) --
Player 1's deck: 9, 5, 6, 4
Player 2's deck: 8, 2, 7, 3, 10, 1
Player 1 plays: 9
Player 2 plays: 8
Player 1 wins round 6 of game 1!

-- Round 7 (Game 1) --
Player 1's deck: 5, 6, 4, 9, 8
Player 2's deck: 2, 7, 3, 10, 1
Player 1 plays: 5
Player 2 plays: 2
Player 1 wins round 7 of game 1!

-- Round 8 (Game 1) --
Player 1's deck: 6, 4, 9, 8, 5, 2
Player 2's deck: 7, 3, 10, 1
Player 1 plays: 6
Player 2 plays: 7
Player 2 wins round 8 of game 1!

-- Round 9 (Game 1) --
Player 1's deck: 4, 9, 8, 5, 2
Player 2's deck: 3, 10, 1, 7, 6
Player 1 plays: 4
Player 2 plays: 3
Playing a sub-game to determine the winner...

=== Game 2 ===

-- Round 1 (Game 2) --
Player 1's deck: 9, 8, 5, 2
Player 2's deck: 10, 1, 7
Player 1 plays: 9
Player 2 plays: 10
Player 2 wins round 1 of game 2!

-- Round 2 (Game 2) --
Player 1's deck: 8, 5, 2
Player 2's deck: 1, 7, 10, 9
Player 1 plays: 8
Player 2 plays: 1
Player 1 wins round 2 of game 2!

-- Round 3 (Game 2) --
Player 1's deck: 5, 2, 8, 1
Player 2's deck: 7, 10, 9
Player 1 plays: 5
Player 2 plays: 7
Player 2 wins round 3 of game 2!

-- Round 4 (Game 2) --
Player 1's deck: 2, 8, 1
Player 2's deck: 10, 9, 7, 5
Player 1 plays: 2
Player 2 plays: 10
Player 2 wins round 4 of game 2!

-- Round 5 (Game 2) --
Player 1's deck: 8, 1
Player 2's deck: 9, 7, 5, 10, 2
Player 1 plays: 8
Player 2 plays: 9
Player 2 wins round 5 of game 2!

-- Round 6 (Game 2) --
Player 1's deck: 1
Player 2's deck: 7, 5, 10, 2, 9, 8
Player 1 plays: 1
Player 2 plays: 7
Player 2 wins round 6 of game 2!
The winner of game 2 is player 2!

...anyway, back to game 1.
Player 2 wins round 9 of game 1!

-- Round 10 (Game 1) --
Player 1's deck: 9, 8, 5, 2
Player 2's deck: 10, 1, 7, 6, 3, 4
Player 1 plays: 9
Player 2 plays: 10
Player 2 wins round 10 of game 1!

-- Round 11 (Game 1) --
Player 1's deck: 8, 5, 2
Player 2's deck: 1, 7, 6, 3, 4, 10, 9
Player 1 plays: 8
Player 2 plays: 1
Player 1 wins round 11 of game 1!

-- Round 12 (Game 1) --
Player 1's deck: 5, 2, 8, 1
Player 2's deck: 7, 6, 3, 4, 10, 9
Player 1 plays: 5
Player 2 plays: 7
Player 2 wins round 12 of game 1!

-- Round 13 (Game 1) --
Player 1's deck: 2, 8, 1
Player 2's deck: 6, 3, 4, 10, 9, 7, 5
Player 1 plays: 2
Player 2 plays: 6
Playing a sub-game to determine the winner...

=== Game 3 ===

-- Round 1 (Game 3) --
Player 1's deck: 8, 1
Player 2's deck: 3, 4, 10, 9, 7, 5
Player 1 plays: 8
Player 2 plays: 3
Player 1 wins round 1 of game 3!

-- Round 2 (Game 3) --
Player 1's deck: 1, 8, 3
Player 2's deck: 4, 10, 9, 7, 5
Player 1 plays: 1
Player 2 plays: 4
Playing a sub-game to determine the winner...

=== Game 4 ===

-- Round 1 (Game 4) --
Player 1's deck: 8
Player 2's deck: 10, 9, 7, 5
Player 1 plays: 8
Player 2 plays: 10
Player 2 wins round 1 of game 4!
The winner of game 4 is player 2!

...anyway, back to game 3.
Player 2 wins round 2 of game 3!

-- Round 3 (Game 3) --
Player 1's deck: 8, 3
Player 2's deck: 10, 9, 7, 5, 4, 1
Player 1 plays: 8
Player 2 plays: 10
Player 2 wins round 3 of game 3!

-- Round 4 (Game 3) --
Player 1's deck: 3
Player 2's deck: 9, 7, 5, 4, 1, 10, 8
Player 1 plays: 3
Player 2 plays: 9
Player 2 wins round 4 of game 3!
The winner of game 3 is player 2!

...anyway, back to game 1.
Player 2 wins round 13 of game 1!

-- Round 14 (Game 1) --
Player 1's deck: 8, 1
Player 2's deck: 3, 4, 10, 9, 7, 5, 6, 2
Player 1 plays: 8
Player 2 plays: 3
Player 1 wins round 14 of game 1!

-- Round 15 (Game 1) --
Player 1's deck: 1, 8, 3
Player 2's deck: 4, 10, 9, 7, 5, 6, 2
Player 1 plays: 1
Player 2 plays: 4
Playing a sub-game to determine the winner...

=== Game 5 ===

-- Round 1 (Game 5) --
Player 1's deck: 8
Player 2's deck: 10, 9, 7, 5
Player 1 plays: 8
Player 2 plays: 10
Player 2 wins round 1 of game 5!
The winner of game 5 is player 2!

...anyway, back to game 1.
Player 2 wins round 15 of game 1!

-- Round 16 (Game 1) --
Player 1's deck: 8, 3
Player 2's deck: 10, 9, 7, 5, 6, 2, 4, 1
Player 1 plays: 8
Player 2 plays: 10
Player 2 wins round 16 of game 1!

-- Round 17 (Game 1) --
Player 1's deck: 3
Player 2's deck: 9, 7, 5, 6, 2, 4, 1, 10, 8
Player 1 plays: 3
Player 2 plays: 9
Player 2 wins round 17 of game 1!
The winner of game 1 is player 2!


== Post-game results ==
Player 1's deck: 
Player 2's deck: 7, 5, 6, 2, 4, 1, 10, 8, 9, 3
```

ゲームの後、勝利したプレイヤーのスコアは、通常のコンバットと同じルールを使用して、元のデッキにあるカードから計算されます。
上記のゲームでは、勝利したプレイヤーのスコアは291です。

筏の船長としての名誉を守るために、前回と同じ2つのデッキを使用して、再帰コンバットのゲームを小さなカニとプレイしてください。
勝利したプレイヤーのスコアはいくつですか？
