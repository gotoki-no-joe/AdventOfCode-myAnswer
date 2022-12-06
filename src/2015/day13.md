# 13日目：食卓の騎士

この数年間、あなたの家族との休暇の饗宴はあまりうまくいっていませんでした。
皆が仲良しという訳にはいかないのです！
今年はそんな状況を変えようとあなたは決心しました。
**最適な座席の配置**を見つけ、それらの気まずい会話をすべてなくすつもりです。

あなたはまず招待したすべての人のリストと、
彼らが隣に誰が座っているときにその幸福度が増減する量を
すべての組み合わせについて書くことから始めました。
全員が快適にちょうど収まる大きさの円形テーブルがあなたの家にあり、
それぞれの人が2人の隣人を持つことになります。

たとえば、予定されている出席者が4人だけで、次のように幸福度を算出したとします。

~~~
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.
~~~

あなたがAliceをDavidの隣に着席させると、Aliceは幸せを2単位を失います。
（Davidがとてもおしゃべりなせいです。）
一方Davidは幸せを46単位得るでしょう。
（Aliceはとても良い聞き手だからです。）
合計で+44です。

テーブルを周って続けて、BobをAliceの隣に座らせるかもしれません。
（Bobは83得て、Aliceは54得る。）
最後にCarolを席に着かせて、隣はBob（Carolは60得て、Bobは7失う）とDavid（Carolは55得て、Davidは41得る）。
配置は次のようになります。

~~~
     +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
     -7  +83
~~~

この仮定のシナリオで、他のすべての座席配置を試すと、
これが最も最適であり、全体の幸せ度が330となることがわかります。

実際のゲストのリストに対する最適な座席配置における
**幸せ度の合計**はいくつですか？

<details><summary>解説</summary><div>

まずこのフォーマットの読み込みから始める必要がある。

|  位置  |  内容  |
| ---: | ---- |
|  0  |  主語  |
|  2  |  gain/lose  |
|  3  |  幸せ度 |
|  10  |  隣人  |

この結果を保存するデータ構造を選ぶのに、本体でどう使うかを考える。
人の名前の順列を作り、隣同士のペアで幸せ度を集計する。
つまり、人の名前の（主語, 隣人）という対をキーとすればよいだろう。

```haskell
parse :: String -> ((String,String), Int)
parse xs = ((w0, init w10), (if w2 == "gain" then id else negate) (read w3))
  where
    [w0,_,w2,w3,_,_,_,_,_,_,w10] = words xs
```

ペアの左を集めて `nub` することで参加者一覧を得て、順列を作り、幸福度の合計を求める。
先頭の一人は固定して構わない。

```haskell
import Data.List
import qualified Data.Map as M

main1 = do
  co <- readFile "input.txt"
  print $ part1 $ lines co

part1 ls = maximum $ map score $ permutations ps
  where
    pqxs = map parse ls
    (p1:ps) = nub $ map (fst . fst) pqxs
    pqxm = M.fromList pqxs
    score ps = sum
      [pqxm M.! (p,q) + pqxm M.! (q,p) | (p,q) <- zip (p1 : ps) (ps ++ [p1])]
```

</div></details>

# パート2

大騒ぎの中で、あなたは自分を座らせるのを忘れていたことに気づきます。
<!-- そういやそうだ。ワロス。-->
この点で、あなたは全体に対してかなり無関心であり、
隣に座っている人によってあなたの幸福度は実際のところ上がったり下がったりしません。
あなたは他の人についても、
誰もがあなたが隣に座っていることについて
同様に無関心である(ambivalent ?)と仮定します。

よって、自分自身をリストに追加して、
あなたに関係するすべての幸福関係にスコア0を与えてください。
<!-- やり方バラしてるやん -->

実際にあなた自身を含む最適な座席配置に対する**幸せ度の合計**はいくつですか？

<details><summary>解説</summary><div>

書いてあるとおりに求める。

```haskell
main2 = do
  co <- readFile "input.txt"
  print $ part2 $ lines co

part2 ls = maximum $ map score $ permutations ps
  where
    pqxs = map parse ls
    ps = nub $ map (fst . fst) pqxs
    pqxm = M.fromList $ pqxs ++ [(pq, 0) | p <- ps, pq <- [("me",p), (p,"me")]]
    score ps = sum
      [pqxm M.! (p,q) + pqxm M.! (q,p) | (p,q) <- zip ("me" : ps) (ps ++ ["me"])]
```

</div></details>
