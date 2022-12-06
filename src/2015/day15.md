# 第15日：飢えた人々のための科学

今日あなたは牛乳浸しクッキーのレシピを完成させる作業を開始しました。
あなたがしなければならないことは、原材料の適切なバランスを見つけることだけです。

あなたのレシピは小さじちょうど100杯の原材料の余地が残っています。
**レシピを完成させるために使用できる残りの原材料**のリスト（パズルの入力）と
小さじ1杯ごとの特性値のリストを作成します。

- `capacity` （容量：クッキーが牛乳を吸収するのをどれくらい助けるか）
- `durability` （耐久：牛乳でいっぱいになったときにクッキーをどのくらい崩れなく保つか）
- `flavor` （風味：クッキーをどれだけ美味しくするか）
- `texture` （口当たり：クッキーの感触をどれだけよくするか）
- `calories` （栄養価：クッキーに追加されるカロリーの量）

あなたは原材料を小さじ1杯単位でのみ量り取ることができ、
また今後も結果を再現できるように正確でなければなりません。
クッキーの**総合点**は、
特性値をそれぞれ合算して（負の合計は0となる）、
カロリー以外のすべてを掛け合わせることで得られます。

たとえば、次の2つの原材料があるとします。

~~~
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
~~~

すると、バタースコッチを小さじ44杯とシナモンを小さじ56杯使う
（各原材料の量は合計して100になる必要があります）
ことを選択すると、
以下の特性値を有するクッキーが得られます。

- 容量 \\(44 \times (-1) + 56 \times 2 = 68\\)
- 耐久 \\(44 \times (-2) + 56 \times 3 = 80\\)
- 風味 \\(44 \times 6 + 56 \times (-2) = 152\\)
- 口当たり \\(44 \times 3 + 56 \times (-1) = 76\\)

これらを掛け合わせると（\\(68 \times 80 \times 152 \times 76\\), ここではカロリーは無視します）、
総合点`62842880`が得られます。
これはこれらの原材料が与えられたときに可能な最高得点です。
いずれかの特性値が負の合計を生成した場合は、
それは零になるので、掛け合わせることで総合点が零になります。

あなたの台所にある原材料その特性値が与えられたとき、
作ることができる最高得点のクッキーの**総合点**はいくらですか？

<details><summary>解説</summary><div>

場合の数が相当になるがともかく、全ての配分を作り出す。
あとk種類の材料で合計wを作る。

```haskell
recipe 1 w = [[w]]
recipe k 0 = [replicate k 0]
recipe k w = [ x : ys | x <- [0..w], ys <- recipe (pred k) (w - x)]
```

配列DPにすると、重複した呼び出しを節約できる。

```haskell
import Data.Array
import Data.Ix

-- numは材料の種類の数
mkRecipe num = recipeA ! (num, 100)
  where
    bnds = ((1,0), (num,100))
    recipeA = array bnds [(kw,recipeF kw) | kw <- range bnds]
    recipeF (1,w) = [[w]]
    recipeF (k,0) = [replicate k 0]
    recipeF (k,w) = [x : ys | x <- [0..w], ys <- recipeA ! (pred k, w - x)]
```

材料ごとに、その特性値がリストのリスト(`list`)で与えられているとして、
`recipes`の各要素について使用量を掛けて足し合わせ、0で足切りした合計を（カロリーを除いて）求め、
その最大値を見つければよい。

```haskell
part1 list recipes =
  maximum
    [ product $ map (max 0) $ init $ foldl1' (zipWith (+)) $ zipWith (\r ps -> map (r *) ps) re list
    | re <- recipes ]
```

このようにリストを読み込む。

```haskell
parse :: String -> [Int]
parse xs = map read (map init [cap,dur,fvr,tex] ++ [cal])
  where
    [_,_,cap,_,dur,_,fvr,_,tex,_,cal] = words xs

main1 = do
  co <- readFile "input.txt"
  let list = map parse $ lines co
  let recipes = mkRecipe $ length list
  print $ part1 list recipes
```

</div></details>

# パート2

あなたのクッキーレシピは大人気になりました！

クッキーひとつがちょうど500カロリーを持つ
別のレシピを作ることができるかどうかを尋ねられました。
（そうすると彼らは食事の代わりとしてそれを使用することができます。）
残りのあなたの受賞歴のあるプロセスは同じにしてください。
（小さじ100杯、同じ材料、同じ採点システム）

例えば、上の材料を考えれば、
代わりに小さじ40杯のバタースコッチと小さじ60杯のシナモンを選択した場合、
（やはり合計で100杯になる必要があります）
合計カロリーは \\($40 \times 8 + 60 \times 3 = 500$になります。
ただし総合点はたった57600000に下がります。
これはこの試行中の状況でできる最高値です。

あなたの台所にある原材料とその特性値が与えられたとき、

を考えると、あなたがの
500カロリーで作ることができる
最高得点のクッキーの**総合点**はいくらですか？

<details><summary>解説</summary><div>

スコアを求める内包表記を改造して、カロリー500のもの限定にする。

```haskell
part2 list recipes =
  maximum
    [ product $ map (max 0) $ init vals
    | re <- recipes
    , let vals = foldl1' (zipWith (+)) $ zipWith (map . (*)) re list
    , last vals == 500
    ]
```

</div></details>
