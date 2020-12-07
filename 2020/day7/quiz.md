# 7日目：手ごろなかばん #

次のフライトに間に合うように地方空港に着陸しました。
実際、食べ物を手に入れる時間さえあるようです。
現在、**荷物の処理の問題**により、すべてのフライトが遅れています。

最近の航空規制により、バッグとその中身について多くの規則（パズルの入力）が施行されています。
バッグは色で判別できる必要があり、特定の数の他の色のバッグが含まれている必要があります。
どうやら、これらの規制の責任者は誰も、それらが施行するのにどれくらいの時間がかかるかを考えていませんでした！

たとえば、次の規則について考えてみます。

~~~
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
~~~

これらの規則は、9色のバッグに必要な内容を指定しています。
この例では、すべての`faded blue`のバッグは空で、
すべての`vibrant plum`バッグには11個のバッグ（`faded blue`5つと`dotted black`6つ）が
含まれている必要があります。

あなたは**`shiny gold`**のバッグを持っています。
これを少なくとも1つの他のバッグに入れて持ち運びたい場合、
最も外側のバッグとして有効なバッグの色はいくつあるでしょうか？
（言い換えれば、いくつの色に最終的に少なくとも1つの`shiny gold`のバッグを入れることができますか？）

上記の規則では、あなたには次の選択肢があります。

- `bright white`のバッグ、これはあなたの`shiny gold`のバッグを直接入れることができます。
- `muted yellow`のバッグ、これはあなたの`shiny gold`のバッグを直接入れることができ、さらに他のバッグをいくつか含みます。
- `dark orange`のバッグ、これは`bright white`と`muted yellow`のバッグを入れることができ、
これらはどちらも`shiny gold`のバッグを入れることができます。
- `light red`のバッグ、これは`bright white`と`muted yellow`のバッグを入れることができ、
これらはどちらも`shiny gold`のバッグを入れることができます。

したがって、この例では、最終的に少なくとも1つの`shiny gold`バッグを含めることができるバッグの色の数は4です。

最終的に少なくとも1つの`shiny gold`バッグを含めることができるバッグの色はいくつですか？
（ルールのリストは非常に長いので、すべてを取得するようにしてください。）

# パート2 #

最近のフライトはかなり高額になっています。
チケットの価格ではなく、購入する必要のあるバッグの数が途方もないためです。

`shiny gold`バッグと上記の例の規則をもう一度考えてみましょう。

- `faded blue`バッグは他のバッグを0個含みます。
- `dotted black`バッグは他のバッグを0個含みます。
- `vibrant plum`バッグは他のバッグを11個含みます。`faded blue`バッグ5つと`dotted black`バッグ6つ。
- `dark olive`バッグは他のバッグを7個含みます。`faded blue`バッグ3つと`dotted black`バッグ4つ。

したがって、1つの`shiny gold`バッグには1つの`dark olive`バッグ（およびその中の7つのバッグ）と
2つの`vibrant plum`バッグ（およびそれらのそれぞれの中に11のバッグ）が含まれている必要があります：
1 + 1*7 + 2 + 2*11=32バッグ！

もちろん、実際の規則は、この例よりも数レベル深くなる可能性がわずかにあります。
入れ子がトポロジー的に現実的でなくなったとしても、必ずすべてのバッグを数えてください！

別の例を次に示します。

~~~
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
~~~

この例では、1つの`shiny gold`バッグは他のバッグが126個入っている必要があります。

1つの`shiny gold`バッグの中にいくつの個別のバッグが必要ですか？
