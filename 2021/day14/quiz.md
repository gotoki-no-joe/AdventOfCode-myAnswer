# 14日目：拡張重合 #

深海に潜ったため、信じられないほどの圧力があなたの潜水艦に負担をかけ始めています。
潜水艦には、潜水艦を補強するのに適した材料を生成する化学反応（重合）装置があり、
近くの活火山の洞窟には、十分な量の必要な原材料があるはずです。

潜水艦のマニュアルには、最適なポリマーの化学式を見つけるための手順が含まれています。
具体的には、**ポリマーテンプレート**と**ペア挿入規則**のリスト（パズル入力）が提供されます。

ペア挿入のプロセスを数回繰り返して得られるポリマーがどのようなものかを理解する必要があります。

例えば：

```
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
```

最初の行は**ポリマーテンプレート**です。これがプロセスの開始点です。

それ以降では、**ペア挿入規則**を定義しています。
`AB -> C` のような規則は、分子 `A` と `B` が隣接している場合、
分子 `C` をそれらの間に挿入する必要があることを意味します。
これらの挿入はすべて同時に行われます。

したがって、ポリマーテンプレート `NNCB` から始めて、
最初のステップでは3つのペアすべてを同時に考慮します。

- 最初のペア (`NN`) は規則 `NN -> C` に一致するため、分子 `C` が1番目と2番目の `N` の間に挿入されます。
- 2番目のペア (`NC`) は規則 `NC -> B` に一致するため、分子 `B` が `N` と `C` の間に挿入されます。
- 3番目のペア (`CB`) は規則 `CB -> H` に一致するため、分子 `H` が `C` と `B` の間に挿入されます。

これらのペアは重なりがあることに注意してください。
ひとつめのペアの2番目の要素は、次のペアの最初の要素です。
また、すべてのペアが同時に考慮されるため、
挿入された要素は次のステップまでペアの一部とは見なされません。

このプロセスの最初のステップの後、ポリマーは `NCNBCHB` になります。

上記の規則を何ステップか適用した結果は次のとおりです。

```
Template:     NNCB
After step 1: NCNBCHB
After step 2: NBCCNBBBCBHCB
After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
```

このポリマーは急速に成長します。
ステップ5の後、長さは97になります。
ステップ10の後、長さは3073になります。
ステップ10の後、`B` は1749回出現し、`C` は298回出現し、`H` は161回出現し、`N` は865回出現します。
最も多い分子の量 (B, 1749) から最も少ない分子の量 (H, 161) を引くと、1749 - 161 = 1588 が得られます。

ポリマーテンプレートへのペア挿入を10ステップ適用し、
結果から最も多い分子と最も少ない分子を見つけます。
**最も多い分子の量から最も少ない分子の量を差し引くと、結果はいくつですか？**

# パート2 #

得られたポリマーは、潜水艦を補強するのに十分な強度ではありません。
ペア挿入プロセスのステップをより多く実行する必要があります。
合計**40ステップ**で目的には足りるでしょう。

上記の例では、最も多い分子は `B`（2192039569602回出現）であり、
最も少ない分子は `H`（3849876073回出現）です。
これらを引くと2188189693529になります。

ポリマーテンプレートへのペア挿入を**40ステップ**適用し、
結果から最も多い分子と最も少ない分子を見つけます。
**最も多い分子の量から最も少ない分子の量を差し引くと、結果はいくつですか？**