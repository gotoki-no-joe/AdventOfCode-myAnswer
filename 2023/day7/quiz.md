# 7日目: ラクダカード

全額自己負担(?)の旅行は、乗船時間5分の飛行船に乗る片道旅行になりました。
（少なくともそれは**かっこいい**飛行船です！）
飛行船は、広大な砂漠の端であなたを降ろし、島之島に戻ります。

「部品は持ってきましたか？」

振り向くと、全身を白い服に包み、ゴーグルを着け、大きなラクダに乗っている妖精の姿がありました。

「部品は持ってきましたか？」彼女はもう一度、今度は大きな声で尋ねました。
彼女がどのパーツを探しているのかわかりません。あなたがここまで来たのは、なぜ砂が止まったのかを解明するためですから。

「部品です！ 砂のための！一緒に来てください。見せてあげますから。」彼女はあなたをラクダの上に手招きします。

砂之島の砂の上を少し走ると、地平線の半分を覆う非常に大きな岩のようなものが見えます。
妖精は、砂之島の島之島の真上にある部分には岩がずっとあり、そこに行くことさえ難しいと説明します。
いつもは、大きな機械を使って岩石を動かしたり砂を濾したりしていますが、
機械の修理に必要な**部品**を最近砂之島が受け取らなくなった(receive 届かなくなった？)ため、機械が故障してしまいました。

部品が止まった理由を解明するのは自分の仕事なのだろうな、と予想したところで彼女が「手伝ってくれないか」と尋ねました。
あなたは習慣的に同意します。

旅には数日かかるため、彼女は**ラクダカード**のゲームを教えてくれると申し出ました。
ラクダカードはポーカーに似ていますが、ラクダに乗りながら遊びやすいように設計されている点が異なります。

ラクダカードは、あなたは**手札**のリストを受け取り、それぞれの手の強さに基づいて手札を順序付けすることが目標です。
手札はA,K,Q,J,T,9,8,7,6,5,4,3,2いずれかがラベル付けされた**5枚のカード**で構成されます。
各カードの相対的な強さはこの順序に従い、Aが最も高く、2が最も低くなります。

全ての手札はちょうど一つの**役**を持ちます。
最も強いものから最も弱いものまで、次のとおりです：

- **ファイブカード**　5枚同じ(Five of a kind) 5枚のカードすべてに同じラベルが付いています。`AAAAA`
- **フォーカード**　4枚のカードが同じラベルを持ち、1枚のカードが異なるラベルを持ちます。`AA8AA`
- **フルハウス**　3枚のカードが同じラベルを持ち、残りの2枚のカードが異なるラベルを共有します。`23332`
- **スリーカード**　3枚のカードが同じラベルを持ち、残りの2枚のカードはそれぞれ手札の他のカードとは異なります。`TTT98`
- **ツーペア**　2枚のカードが1つのラベルを共有し、他の2枚のカードが2番目のラベルを共有し、残りのカードに3番目のラベルが付いています。`23432`
- **ワンペア**　2枚のカードが1つのラベルを共有し、他の3枚のカードにはペアとも互いに異なるラベルを持ちます。`A23A4`
- **ハイカード**　全てのカードのラベルが異なります。`23456`

手札は主にタイプに基づいて順序付けされます。
例えば、全ての**フルハウス**はどの**スリーカード**よりも強いです。

2つの手札が同じ役の場合、2番目の順序付けルールが有効になります。
まず、**それぞれの手札の最初のカード**を比較します。
これらのカードが異なる場合は、最初のカードが強い手札が強いとみなされます。
ただし、両手札の最初のカードの**ラベルが同じ**である場合は、**各手札の2番目のカード**の検討に進みます。
それらが異なる場合は、2番目のカードの高い方が勝ちます。
それ以外の場合は、各手札の3番目のカード、次に4番目、次に5番目のカードと続けます。

つまり、`33332`と`2AAAA`は両方とも**フォーカード**の役ですが、最初のカードがより強いため、`33332`の方が強いです。
同様に、`77888`と`77788`はどちらも**フルハウス**ですが、3番目のカードがより強い（そして、両方の手札の1番目と2番目のカードが同じ）ため、`77888`の方が強いです。

ラクダカードを遊ぶには、手札とそれに対応する入札のリスト（あなたのパズル入力）が与えられます。
例えば：

```
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
```

この例は5つの手札を示しています。
それぞれの手札の後には**入札**額が続きます。
それぞれの手札、その入札額にその**順位**を乗じた額に等しい金額を獲得します。
最も弱い手札は順位1を得て、2番目に弱い手札は順位2を得て、以下同様に最も強い手札まで続きます。
この例には5つの手札があるため、最も強い手札は順位5を持ち、その入札額の5倍が得られます。

したがって、最初のステップは、手札を強さの順に並べることです：

- `32T3K` は唯一の**ワンペア**で、他の手札は全てより強い役であるため、これが順位1になります。
- `KK677` と `KTJJT` は両方とも**ツーペア**です。
最初のカードはどちらも同じラベルが付いていますが、`KK677` の2番目のカードの方が強い（`K` 対 `T`）ため、
`KTJJT`が順位2を得て、`KK677`が順位3になります
- `T55J5` と `QQQJA` はどちらも**スリーカード**です。
`QQQJA`の方が最初のカードが強いため、これが順位5となり、`T55J5`は順位4となります。

では、各手札の入札額とその順位を乗算した結果を合計する
($765 \times 1 + 220 \times 2 + 28 \times 3 + 684 \times 4 + 483 \times 5$)ことで、
この手札セットの合計賞金を決定できます。
したがって、この例での**賞金の合計**は**6440**です。

セット内のすべての手札の順位を求めます。**賞金総額はいくらですか？**

# パート2

物事をもう少し面白くするために、妖精は追加のルールを1つ導入します。
今や、`J`のカードはジョーカーです。
このワイルドカードは、手札を可能な限り最強の役にするカードとして働きます。

これとバランスを取るために、**`J`カードは個別のカードとしては最も弱く**、`2`よりもさらに弱いものになります。
他のカードの順序は同じままです: `A`, `K`, `Q`, `T`, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`, `J`

`J`カードは、手札の役を決定するために最適なカードのふりをすることができます。
例えば、`QJJQ2` は今や**フォーカード**とみなされます。
ただし、同じ役の2つの手札間の決戦の目的では、`J`はそのふりをしているカードではなく、常に`J`そのものとして扱われます。
つまり、`J`は`Q`より弱いため、`JKKK2`は`QQQQ2`より弱いです。

さて、上記の例は大きく変わります：

```
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
```

- `32T3K`はやはり唯一の**ワンペア**です。ジョーカーが入っていないので強さは上がりません。
- `KK677`は現在唯一の**ツーペア**であり、2番目に弱い手札となっています。
- `T55J5`, `KTJJT`, `QQQJA` は全て**フォーカード**になりました！
`T55J5`が順位3、`QQQJA`が順位4、`KTJJT`が順位5を取得します。

新しいジョーカールールでは、この例の賞金の合計は**5905**です。

新しいジョーカールールを使用して、セット内の全ての手札の順位を求めます。
**新しい賞金総額はいくらですか？**