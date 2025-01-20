行 `theRow` 列 `theCol` の値を聞かれている。
まず、それが数列の何個目の値かを、ひとつめの表を見ながら考える。

斜めの列の \\(K\\) 本めを考える。
それぞれの長さは順に \\(1,2,3,\dots\\) なので、その末尾の数は \\(1 + 2 + \dots + K = K (K + 1) / 2\\) となる。
またそれは行\\(1\\)列\\(K\\)の位置にある。

行 \\(r\\) 列\\(c\\) のマスは、そこから右上マスに\\(r-1\\)回移動した先にある、行\\(1\\)列\\(c+r-1\\)と同じ斜め列に属する。つまり \\(K = c + r - 1\\) 行に属する。
そのひとつ手前、\\(K-1\\)本めの斜め列の最後のマスの番号は \\((K-1)K/2\\) で、
行\\(r\\)列\\(c\\)のマスはそれからさらに\\(c\\)マス先にあるので \\((c+r-2)(c+r-1)/2 + c\\) 番である。

```haskell
index r c = div ((c + r - 2) * (c + r - 1)) 2 + c
```

次に、数列の `index theRow theCol` 番めの値を求める。
第1項は `20151125`, 直前の項から次の項を得る漸化式から列を作る。

```haskell
theSeed = 20151125
theMag  = 252533
modBase = 33554393

step :: Int -> Int
step x = mod (x * theMag) modBase

theSeq = 0 : iterate step theSeed

part1 = theSeq (index theRow theCol)
```

しかし計算が終わらない。繰り返し回数が大きすぎる。

ここで、数列の定義をもう一度見てみる。

\\(a_1 = 20151125\\)  
\\(a_{i+1} = a_i \times 252533 \bmod 33554393\\)

剰余で考えればよいので、これはモジュロ演算で

\\(a_i \equiv 20151125 \times 252533^{i-1} \bmod 33554393\\)

と等しい。このべき乗は、2進数に基づいて高速に計算する定番の方法がある。

```haskell
-- @gotoki_no_joe
powerish mul i a b = foldl' mul i [p | (True, p) <- zip bs ps]
  where
    bs = map odd $ takeWhile (0 <) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a

part1a = powerish mul theSeed theMag $ pred $ index theRow theCol

mul x y = mod (x * y) modBase
```
