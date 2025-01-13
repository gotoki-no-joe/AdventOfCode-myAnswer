# パート1

装備のパラメータを (金額, 攻撃力, 防御力) というタプルで表す。

お買い物の全ての場合をリスト内包表記で数え上げてみる。

武器はひとつだけ選んで購入する。

```haskell
    weapon <- [(8,4,0),(10,5,0),(25,6,0),(40,7,0),(74,8,0)]
```

防具はひとつだけ選んで購入するか、使わないでもよい。

```haskell
    armor <- [(0,0,0),(13,0,1),(31,0,2),(53,0,3),(75,0,4),(102,0,5)]
```

指輪は二つまで使える。

```haskell
rings = [(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3),(0,0,0)]

    (ring1:rs) <- tails rings
    ring2 <- if null rs then [ring1] else rs
```

総当たりの組み合わせを、費用の安い順にする。

```haskell
cags = sort
  [ weapon `add` armor `add` ring1 `add` ring2
  | weapon <- ...
  ]

add (a,b,c) (d,e,f) = (a+d,b+e,c+f)
```

自分のHP、攻撃力、防御力を \\(h_1 = 100, a_1, g_1\\)、
ボスのそれを \\(h_2, a_2, g_2\\) とする。

こちらの攻撃1回で削るHP量は \\(\max(1, a_1-g_2)\\) で、
ボスを倒すまでかかるターン数は \\(\lceil \frac{h_2}{\max(1, a_1 - g_2)} \rceil\\) である。

ボスからの攻撃1回で削られるHP量は \\(\max(1, a_2-g_1)\\) で、
耐えるターン数は \\(\lceil \frac{h_1}{\max(1, a_1-g_2)} \rceil\\) である。

結局、この戦いに勝つ条件は
\\(\lceil \frac{h_1}{\max(1, a_1 - g_2)} \rceil \geq \lceil \frac{h_2}{\max(1, a_1 - g_2)} \rceil\\)
となる。

```haskell
win a1 g1 = divrup h1 (max 1 (a2 - g1)) >= divrup h2 (max 1 (a1 - g2))

h2 = ...
a2 = ...
g2 = ... -- 入力データ

part1 = head $ filter (\(_,a1,g1) -> win a1 g1) cags
```

# パート2

つまり上の逆順に、負ける最大の金額を探せばよい。

```haskell
part2 = head $ filter (\(_,a1,g1) -> not $ win a1 g1) $ reverse cags
```

<!--
一般論で言えば `filter ... . reverse` は
`filter` で要素を減らしてから `reverse` する方がお得かもしれないが、
もしここで `win` の計算がとても重いような場合は、先に `reverse` する方がマシなので、
効率的なプログラムの構造は一意ではないぞ、とエディタのアシストに言いたい。
-->
