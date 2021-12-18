# 18日目：クサウオ #

あなたは海溝に降りて、
深海魚の[クサウオ](https://ja.wikipedia.org/wiki/%E3%82%AF%E3%82%B5%E3%82%A6%E3%82%AA%E7%A7%91)に遭遇します。
彼らはそりの鍵を見たと言います！
クサウオの子供の算数の宿題を手伝うなら、鍵がどちらの方に行ったのか教えてくれるとさえ言いました。

クサウオの使う数は通常の数とは異なります。
すべてのクサウオの数は**対**、つまり2要素の順序付きリストです。
対の各要素は、通常の数または別の対のいずれかになります。
（訳注：regular number はハミング数も指す言葉だが、ここではおそらく普通の自然数のこと。）

対は `[x,y]` の形式で表記します。
ここで `x` と `y` は対の中のの要素です。
クサウオ数の例を次に示します。1行にクサウオ数ひとつです。

```
[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
```

このクサウオの宿題は**足し算**です。
2つのクサウオ数を加算するには、加算演算子の左右の引数から対を作ります。
たとえば、`[1,2]+[[3,4],5]` は`[[1,2],[[3,4],5]]` になります。

問題が1つだけあります。
**クサウオ数は必ず約分する必要があります。**
そして、2つのクサウオ数を加算する手順は、結果が約分の必要があるものになることがあります。

**クサウオ数を約分するには**、
クサウオ数に適用される操作の以下のリストから、
可能な最初のものを繰り返し適用する必要があります。

- いずれかの対が**4つの対の中に入れ子になっている**場合、そのような最も左の対が**爆発**します。
（訳注：追加の条件あり）
- いずれかの通常の数が**10以上**の場合、そのような最も左の数を**分割**します。

上記のリストの操作がどちらも適用できないとき、クサウオ数の約分が完了しました。

約分の手順において、操作はたかだか1度適用し、その後、手順は操作のリストの先頭に戻ります。
たとえば、**分割**により**爆発**の基準を満たす対を生成したならば、
他の**分割**を行う前にその**爆発**を行います。
（訳注、爆発できる箇所なし、分割できる箇所2つ、というクサウオ数があって、
左の方を分割した結果、爆発が行えるようになったならば、右の分割よりも爆発が優先される、
ということ。）

対を**爆発**させるには、
対の左側の値を、爆発させる対の左側の最初の通常の数（存在する場合）に足し込み、
対の右側の値を、爆発させる対の右側の最初の通常の数（存在する場合）に足しこみます。
爆発させる対は、常に2つの通常の数で構成されます。
次に、爆発させる対全体を通常数0に置き換えます。

単一の爆発操作の例を次に示します。
（原文では爆発する対と影響を受けた箇所が強調されています）

- `[[[[[9,8],1],2],3],4]` は `[[[[0,9],2],3],4]` になります。（9の左側に通常の数がないため、それはどの数にも足されません。）
- `[7,[6,[5,[4,[3,2]]]]]` は `[7,[6,[5,[7,0]]]]` になります。（2の右側に通常の数がないため、それはどの数にも足されません。）
- `[[6,[5,[4,[3,2]]]],1]` は `[[6,[5,[7,0]]],3]` になります。
- `[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]` は `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` になります。
（対 `[3,2]` は影響を受けません。
対 `[7,3]` がそれよりも左側にあるためです。
対 `[3,2]` は次の操作で爆発します。）
- `[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]` は `[[3,[2,[8,0]]],[9,[5,[7,0]]]]` になります。

通常の数を**分割**するには、それを対で置き換えます。
対の左の要素は、その数を2で割って**切り捨て**で丸めた結果とします。
右の要素は、その数を2で割って**切り上げ**で丸めた結果とします。
たとえば、10は `[5,5]` になり、11は `[5,6]` になり、12は `[6,6]` になります。

`[[[[4,3],4],4],[7,[[8,4],9]]] + [1,1]` の約分済みの結果を得る手順は次のとおりです。
（原文には強調表示があります。）

```
加算後: [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]     -- [4,3]
爆発後:  [[[[0,7],4],[7,[[8,4],9]]],[1,1]]        -- [8,4]
爆発後:  [[[[0,7],4],[15,[0,13]]],[1,1]]          -- 15
分割後:    [[[[0,7],4],[[7,8],[0,13]]],[1,1]]     -- 13
分割後:    [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]  -- [6,7]
爆発後:  [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
```

約分操作が適用できなくなったときのクサウオ数 `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]` が、
足し算の実際の結果です。

宿題では、**クサウオ数のリスト**（パズルの入力）を足し合わせることが求められています。
クサウオ数は1つが1行に記載されています。
最初のクサウオ数と2番目のそれを足し、次にその結果と3番目を足し、次にその結果と4番目を足し、
以下同様にリスト内のすべての数を一度使用するまで続けます。

たとえば、次のリストの最終的な合計は `[[[[1,1],[2,2]],[3,3]],[4,4]]` です。

```
[1,1]
[2,2]
[3,3]
[4,4]
```

次のリストの最終的な合計は `[[[[3,0],[5,3]],[4,4]],[5,5]]` です。

```
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
```

次のリストの最終的な合計は `[[[[5,0],[7,4]],[5,5]],[6,6]]` です。

```
[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
```

少し大きい例を次に示します。

```
[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]
```

最終的な合計 `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` は、
上記のクサウオ数を全て足し上げることで求められます。

```
  [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
+ [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]

  [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
+ [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]

  [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
+ [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
= [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]

  [[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]
+ [7,[5,[[3,8],[1,4]]]]
= [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]

  [[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]
+ [[2,[2,2]],[8,[8,1]]]
= [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]

  [[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]
+ [2,9]
= [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]

  [[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]
+ [1,[[[9,3],9],[[9,0],[0,7]]]]
= [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]

  [[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]
+ [[[5,[7,4]],7],1]
= [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]

  [[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]
+ [[[[4,2],2],6],[8,7]]
= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
```

それが正しい答えであるかどうかを確認するために、
クサウオの先生は最終的な合計の**大きさ**だけを確認します。
対の大きさは、左の要素の大きさの3倍に右の要素の大きさの2倍を加えたものです。
通常の数の大きさは単にその数です。

たとえば、 `[9,1]` の大きさは `3*9 + 2*1 = 29` です。
`[1,9]` の大きさは `3*1 + 2*9 = 21` です。
大きさの計算は再帰的です。
`[[9,1],[1,9]]` の大きさは `3*29 + 2*21 = 129` です。

次に、大きさの例をいくつか示します。

- `[[1,2],[[3,4],5]]` は143になります。
- `[[[[0,7],4],[[7,8],[6,0]]],[8,1]]` は1384になります。
- `[[[[1,1],[2,2]],[3,3]],[4,4]]` は445になります。
- `[[[[3,0],[5,3]],[4,4]],[5,5]]` は791になります。
- `[[[[5,0],[7,4]],[5,5]],[6,6]]` は1137になります。
- `[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]` は3488になります。

したがって、下の例の宿題を考えると、

```
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
```

最終的な合計は次のとおりです。

```
[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]
```

この最終的な合計の大きさは4140です。

宿題のクサウオ数を、示されている順にすべて足し合わせます。
**最終的な合計の大きさはいくつですか？**

# パート2 #

あなたは宿題のプリントの裏にある2番目の問いに気づきました。

クサウオ数を2つだけ足した結果の最大の大きさはどれくらいですか？

クサウオ数の足し算は[可換](https://ja.wikipedia.org/wiki/%E4%BA%A4%E6%8F%9B%E6%B3%95%E5%89%87)でないことに注意してください。
つまり、 `x + y` と `y + x` の結果は異なることがあります。

上記の最後の宿題の例をもう一度考えてみましょう。

```
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
```

このリストの任意の2つのクサウオ数の合計の最大の大きさは3993です。
これは `[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]] + [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]`
の大きさであり、約分すると
`[[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]]`
になります。

**宿題にある2つの異なるクサウオ数の輪の大きさの最大値はいくつですか？**