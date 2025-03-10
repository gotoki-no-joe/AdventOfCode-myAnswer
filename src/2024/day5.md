# 5日目:印刷キュー

ケレスでの捜索に満足した学者集団は、次に、地下17階の文房具の山をスキャンすることを提案します。

北極印刷局はクリスマスに近づくにつれてかつてないほど忙しくなり、
歴史学者たちがこの歴史的に重要な施設の捜索を続ける一方で、
[非常に馴染みのあるプリンタ](../2017/day1.md)を操作している小人があなたを手招きしています。

小人はあなたが誰か思い出したのでしょう。<!-- 超訳 -->
なぜなら、彼らは新しい**そりの打ち上げ安全マニュアル**の更新が正しく印刷できないことを説明する時間も惜しいからです。
安全マニュアルを更新しないことは本当に悲惨なことなので、あなたは助けを申し出ます。

安全プロトコルは、安全マニュアルの新しいページが**非常に特定の順序で**印刷されなければならないことを明確に示しています。
`X|Y` という表記は、ページ番号 `X` とページ番号 `Y` の両方が更新の一部として生成される場合、
ページ番号 `X` はページ番号 `Y` の前のある時点で印刷される**必要**があることを意味します。

小人は、**ページ順序の規則**と各更新で生成されるページ（あなたのパズル入力）の両方がありますが、
各更新でページが正しい順序であるかどうかを判断することはできません。

例えば：

```
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
```

最初のセクションでは、1行に1つずつ、**ページ順序の規則**を指定しています。
最初の規則 `47|53` は、更新にページ番号47とページ番号53の両方が含まれている場合、
ページ番号47はページ番号53よりも前のある時点で印刷する必要があることを意味します。
（47は必ずしも53の**直前**である必要はありません。他のページが53との間にあってもかまいません。）

2番目のセクションでは、各更新のページ番号を指定しています。
ほとんどの安全マニュアルは異なるため、更新に必要なページも異なります。
最初の更新 `75, 47, 61, 53, 29` は、
更新がページ番号 75, 47, 61, 53, 29 から構成されていることを意味します。

できるだけ早くプリンタを稼働させるために、
**既に正しい順序になっている更新はどれか**を特定することから始めます。

上の例では、最初の更新( `75,47,61,53,29` )は正しい順序です。

- 75は、その他のページをより後ろに配置するルール(`75|47`, `75|61`, `75|53`, `75|29`) があるため、正しく先頭になります。
- 47は、75がその前になければならず(`75|47`)、`47|61`, `47|53`, `47|29` よりその他のページは後ろにあるべきなので、正しく2番目です。
- 61は、75と47がその前にあり(`75|61`と`47|61`)、53と29がその後にある(`61|53`と`61|29`)ため、正しく中央です。
- 53は、ページ番号29の前にあるため、正しく4番目です(`53|29`)。
- 29は残っている唯一のページであり、正しく最後です。

最初の更新には一部のページ番号が含まれないため、欠落したページ番号に関する順序ルールは無視されます。

2回目と3回目の更新も、ルールに則った正しい順序です。
1回目の更新と同様に、全てのページ番号が含まれるわけではないため、一部の順序ルールのみが適用されます。
各更新では、欠落したページ番号を含む順序ルールは使用されません。

4番目の更新 `75,97,47,61,53` は正しい順序ではありません。
97の前に75が印刷され、`97|75` の規則に違反します。

5番目の更新 `61,13,29`  も、ルール `29|13` に違反しているため、正しい順序ではありません。

最後の更新 `97,13,75,29,47` は、いくつかの規則に違反しているため、正しい順序ではありません。

何らかの理由で、小人は印刷される各更新の**中央のページ番号**も知る必要があります。
現在は正しい順序の更新のみを印刷しているため、正しい順序の各更新の中央のページ番号を見つける必要があります。
前述の例では、正しい順序の更新は次のとおりです:

```
75,47,61,53,29
97,61,53,29,13
75,29,13
```

これらの中央のページ番号はそれぞれ 61, 53, 29 です。
これらのページ番号を合計すると143になります。

もちろん、注意が必要です。
実際の**ページ順序付けルール**のリストは、上記の例よりも大きく複雑です。

どの更新が既に正しい順序になっているかを判定します。
正しい順序の更新の中央のページ番号を合計するといくつになりますか？

# パート2

小人が正しい順序の更新を印刷する作業を開始している間に、残りの更新を修正する時間が少しあります。

**順序が正しくない更新**それぞれについて、ページ順序ルールを使用してページ番号を正しい順序に配置します。
上の例について、順序が正しくない3つの更新とその正しい順序を次に示します：

- `75,97,47,61,53` は `97,75,47,61,53` になります。
- `61,13,29` は `61,29,13` になります。
- `97,13,75,29,47` は `97,75,47,29,13` になります。

**誤った順序の更新**のみを選び、それらを正しく順序付けると、それらの中央のページ番号は 47, 29, 47 になります。
これらを合計すると、123になります。

正しい順序ではない更新を見つけます。
それらの更新だけを正しく順序付けた後、中央のページ番号を合計するといくつになりますか？
