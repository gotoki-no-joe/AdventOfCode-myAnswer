ロボの列があり、
一番先のロボのアームについて基本動作、つまり上下左右Aを行わせるために、
一番手前のカーソルキーを操作する**回数**がどうなるかを考える。
というかそれがそれぞれ求められたとする。

そういうロボの列を使って、もう一つ前にロボを置き、
それに各基本動作をさせるために必要な操作の回数を求める。

という向きでは、全ての状態から始めないといけないし、系列そのものが必要になってわけわからん？

------

テンキードアの上に k 枚カーソルロボが重なった状態で、
それぞれのコードを入力する最短系列を作り、
それを実行する最短経路を作り、という系列を実際に作る方法では、
パート1で、20文字のコードを入力する操作がロボ2段で6桁の数になっていることを考えると、
毎回100倍になるとして、25段のロボではそんな桁数になってしまうから、
操作自体はもう作らなくていいアプローチが必要。

ロボの列の前にもう一つロボを置き、それに
カーソルロボの各操作をさせる最短手順
テンキードアの各操作をさせる最短手順
のどちらかを数える、という感じ？

-----

カーソルロボを操作することだけ考える。

カーソルロボの系列を使って、一番先頭のロボに、
「その前に置いたカーソルロボに、そのさらに前にあるカーソルロボの操作をする」
おかしい？

----

カーソルロボの系列がある。
Aに手があるリセット状態から始めて、

----

テンキードアの視点から、
「自分についてこれこれの操作をさせるための操作系列の長さ」
を答えよ、という再帰呼び出しを重ねて、
これを何かの視点でDPする感じ？

029A を、

---

25日めも書き終えて、ここで諦めると再開は難しいとおもったので、MS copilot に聞いてみた。

Day 21 of Advent of Code 2024, titled "Keypad Conundrum," can be quite challenging! Here are a few hints to help you with part 2:

1. **Understand the Keypads**: You need to work with two different keypads. Make sure you clearly understand the layout and how to navigate each one.

2. **Recursion and Memoization**: These techniques can be very helpful. Recursion will help you explore all possible paths, and memoization will save you from recalculating paths you've already explored[1](https://todd.ginsberg.com/post/advent-of-code/2024/day21/).

3. **Shortest Paths**: Focus on finding the shortest paths between points on the keypads. This will involve calculating the cost of each path and choosing the minimum[1](https://todd.ginsberg.com/post/advent-of-code/2024/day21/).

4. **Path Structures**: Define your path structures clearly. This will help you keep track of the movements and ensure you are following the correct paths[1](https://todd.ginsberg.com/post/advent-of-code/2024/day21/).

If you need more detailed guidance, you might find it helpful to look at some example solutions or discussions on platforms like Reddit or GitHub. Good luck, and happy coding! If you have any specific questions, feel free to ask.

https://todd.ginsberg.com/post/advent-of-code/2024/day21/


うん、もうこれを全面的に頼ろう。すごくよく書けている説明っぽい。

---- 捨てた下書き



# ---方針


いずれかのキーパッドに相対しているロボが、そのキーパッドで何らかのボタン列を打ち込みたいとき、
「現在アームはどのキーの上にあるか」により「次に押したいボタンに移動し、それを押す」という操作の最適な系列は変化する。
また、現在位置と目標位置の一つの組み合わせに関して、複数の最適な系列が存在しうる。

テンキーバッドに関して、（`input.txt` にあるような）ひとつのドアコードを入力するために、
テンキーバッドの前にいるロボ1を操作する最適な（方向キーパッドの）操作列が
前後するボタンの組み合わせから（複数の可能性を持ちつつ）構成できる。

次に、ロボ1の方向キーパッドを操作するロボ2で、ロボ1にそのような操作列を入力するための最適な操作列が
前後するボタンの組み合わせから（複数の可能性を持ちつつ）構成できる。

以下、設置したロボの台数分だけこれを繰り返し、自分の立っている位置では、
その操作列を入力する手数は、操作列の長さそのものになる。

これを順次持って戻ると、「ロボxxがこのように動くための、人が操作する最適な操作系列の長さ（以下コスト）」が順に求まり、
一番上では「ドアのテンキーにコードを打ち込むための最小コスト」が得られる。

つまり、それぞれのロボに対応して、次のようなシグネチャを持つコスト算出関数が立てられる。

```haskell
type CostFunc
  =  String  -- code : 操作系列
  -> Int     -- codeをこのロボが打ち込むための最小コスト
```

パート1の構成で、一つのドアコードに対して最終的なコストを求める関数が次のように定義できる。

```haskell
cost4part1 = findCost3
  where
    findCost0, findCost1, findCost2, findCost3 :: CostFunc
    findCost3 = ...(findCost2)... -- ドアの前のロボ。数字キーパッドを操作する。下請けに次を使う
    findCost2 = ...(findCost1)... -- 次のロボ。方向キーパッドを操作する。下請けに次を使う
    findCost1 = ...(findCost0)... -- 次のロボ。方向キーパッドを操作する。下請けに次を使う
    findCost0 = length            -- 自分のいる位置
```

中間の 1～3はコードはほぼ同様で、キーパッドの内容に基づき操作列を生成し、
その操作列を下請けで算出したコストの最小値の総和を返す。
そのうち一番先頭だけ、参照するキーバッドが異なる。
一番下は、単なる `length` になる。
なので、これらの関数は一つの定義でまかなえる。

```haskell
cost4part1 = findCost 3
  where
    findCost 0     code = length code -- 一番手前は自分が直接操作
    findCost depth code = ...(findCost (pred depth))... -- 中間の計算は共通
      where
        pad = if depth == 3 then numericPad else directionPad -- 一番奥は数字キーパッドを操作する
```

## DP

上の `findCost` は、特にパート2では25段の再帰になり、重すぎて計算が終わらない。
メモ化により高速化することを考える。
引数が `code` と `depth` の二つあるので、ペアで持たせることにする。
計算する必要のある引数の内容は、静的には構成できず、実行してみないとわからないので、
