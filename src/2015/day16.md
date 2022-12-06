# 16日目：スーおばさん

あなたのスーおばさんはあなたに素晴らしい贈り物をくれました。
そこであなたは彼女に感謝の手紙を送りたいのです。
しかし、ちょっとした問題があります。
彼女は「スーおばさんより」と記名していました。

あなたには「スー」という名前のおばが500人います。

だから、間違った人に手紙を送るのを避けるために、
あなたはどのスーおばさん
（あなたは自分の正気を保つために便宜上1から500の番号を付けています）
があなたに贈り物をしたのかを知る必要があります。
あなたは贈り物を開けます。なんと運のいいことでしょう。
古きよきスーおばさんはあなたにマイファースト犯罪現場分析機をくれました！
まさにあなたが欲しいもの、あるいは必要なもの、です。

マイファースト犯罪現場分析機
（My First Crime Scene Analysis Machine, 略してMFCSAM）は、
与えられたサンプル中のいくつかの特定の化合物、
およびそれらの化合物の種類がいくつあるかを検出することができます。
説明書によると、MFCSAMが検出できるものは以下のとおりです。

- `children`（子供） 人間のDNA年齢分析ができます。
- `cats`（猫） 個々の品種は区別できません。
- いくつかの一見ランダムな犬の品種：
`samoyeds`（サモエド）, `pomeranians`（ポメラニアン）,
`akitas`（秋田犬）, `vizslas`（ショートヘアード・ハンガリアン・ビズラ）
- `goldfish`（金魚）それ以外の魚は検出できません。
- `trees` （木）1つのグループにまとめられています。
- `cars`（車）おそらく排気ガスかガソリンか何かによって。
- `perfumes`（香水）あなたのスーおばさんの多くはいくつかを付けているので、これは便利です。

実際には、あなたのスーおばさんの多くはこれらの多くを持っています。
あなたは贈り物の包装をMFCSAMに入れました。
それはあなたに数回不思議にビープ音を鳴らしてから
紙テープ(ticker tape)にメッセージをプリントアウトします。

~~~
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
~~~

あなたはそれぞれのスーおばさんについて、思い出せたもののリストを作ります。
あなたのリストには欠けているものは、
あなたが単に数を覚えていなかいということで、
ゼロだという意味ではありません。

あなたに贈り物をしたスーおばさんの**番号**はいくつですか？

<details><summary>解説</summary><div>

物の名前と、整数が対になっている。
思い出したメモの内容で、MFCSAMの情報と矛盾がないものを選択する。
おばさんの情報は `Data.Map.Map String Int` の形式で扱う。

```haskell
import qualified Data.Map as M
import Data.Maybe

mfcsam =
  [("children:",3),("cats:",7),("samoyeds:",2),("pomeranians:",3),("akitas:",0)
  ,("vizslas:",0),("goldfish:",5),("trees:",3),("cars:",2),("perfumes:",1)]

part1 aunts = [id | (id, aunt) <- zip [1..] aunts, all (check aunt) mfcsam]

check aunt (k,v) = maybe True (v ==) $ M.lookup k aunt

parse xs = M.fromList $ loop ws
  where
    ws = drop 2 $ words xs
    loop [k,v] = [(k, read v)] -- 最後だけコンマがないため
    loop (k:v:ws) = (k, read $ init v) : loop ws

main1 = do
  co <- readFile "input.txt"
  let aunts = map parse $ lines co
  print $ part1 aunts
```

</div></details>

# パート2

あなたがお礼状を送ろうとしたそのとき、
MFCSAMの説明書の中の何かがあなたの目を引きます。

どうやら、それは旧式のテム＝レイサーキット
（原文では[retroencabulator](https://www.youtube.com/watch?v=RXJKdh1KZ0w), [Wikipedia](https://en.wikipedia.org/wiki/Turboencabulator)）
を使っているため、機械からの出力は正確な値ではありません。
それらのいくつかは範囲を示しています。

特に、`cats`と`trees`の読み取り値は、
それ**より多く**のものがあることを示しています
（猫のフケと木の花粉の予測できない核崩壊のため）、
一方`pomeranians`と`goldfish`の読み取り値は、
それ**より少ない**ことを示しています
（磁気抵抗のモード相互作用による） 。
(modial?)

本物のスーおばさんの**番号**は何でしょう？

<details><summary>解説</summary><div>

比較関数を妥当なものに交換しつつ検査する形に変更する。

```haskell
part2 =
  [ id
  | (id, aunt) <- zip [1..] aunts
  , all (check   aunt) mfcsamEQ
  , all (checkLT aunt) mfcsamLT
  , all (checkGT aunt) mfcsamGT]

checkLT aunt (k,v) = maybe True (v >) $ M.lookup k aunt
checkGT aunt (k,v) = maybe True (v <) $ M.lookup k aunt

mfcsamEQ = [("children:",3),("samoyeds:",2),("akitas:",0)
           ,("vizslas:",0),("cars:",2),("perfumes:",1)]
mfcsamLT = [("pomeranians:",3),("goldfish:",5)]
mfcsamGT = [("cats:",7),("trees:",3)]

main2 = readFile "input.txt" >>= print . part2 . map parse . lines
```

</div></details>
