# 解説 #

テクニックの内容をどう計算するかを色々試すので、
はやる心を抑えてテクニックを表すデータ型をまずは定義する。
（後々大きな整数を扱って、64bit Intでは溢れるので
先回りしてIntegerで型宣言している。）

```haskell
data Shuffle = Deal           -- deal into new stack
             | CutN  Integer  -- cut N
             | DealN Integer  -- deal with increment N
```

パズル入力の行を解釈して`Shuffle`型に直すパーサも必要になる。

```haskell
parse :: String -> Shuffle
parse s = case words s of
    "cut" : arg : _ -> CutN (read arg)
    "deal" : "with" : _ : arg : _ -> DealN (read arg)
    "deal" : "into" : _ -> Deal
    _ -> error "parse fail"
```

各サンプルが値として表現できる。

```haskell
sample1, sample2, sample3, sample4 :: [Shuffle]
sample1 = [DealN 7, Deal, Deal]
sample2 = [CutN 6, DealN 7, Deal]
sample3 = [DealN 7, DealN 9, CutN (-2)]
sample4 = [Deal, CutN (-2), DealN 7, CutN 8, CutN (-4), DealN 7, CutN 3, DealN 9, DealN 3, CutN (-1)]
```

まず、ナイーブにリストを使ってデッキの状態を忠実にシミュレーションする方法が考えられる。
`Deal`は`reverse`、`CutN`は`splitAt`して`(++)`、`DealN`は新しい位置を忠実に計算する。
デッキの枚数`base`を大域的な情報として必要とする。

```haskell
type Deck = [Integer]

naiveInterp :: Integer -> Shuffle -> (Deck -> Deck)
naiveInterp _     Deal     = reverse
naiveInterp base (CutN n)  = \xs -> let (as,bs) = splitAt (fromIntegral $ mod n base) xs in bs ++ as
naiveInterp base (DealN n) = \xs -> map snd $ sort [(j,x) | (i,x) <- zip [0..] xs, let j = i * n `mod` base]

naiveRun :: Integer -> [Shuffle] -> Deck
naiveRun base ss = foldl (flip ($)) [0..base - 1] (map (naiveInterp 10) ss)
```

```
*Main> naiveRun 10 sample1
[0,3,6,9,2,5,8,1,4,7]
*Main> naiveRun 10 sample2
[3,0,7,4,1,8,5,2,9,6]
*Main> naiveRun 10 sample3
[6,3,0,7,4,1,8,5,2,9]
*Main> naiveRun 10 sample4
[9,2,5,8,1,4,7,0,3,6]
```

ではパート1の本番を計算しよう。

```haskell
base1 :: Integer
base1 = 10007

main1 = do
    co <- readFile "input.txt"
    let ans = findIndex (2019 ==) $ naiveRun base1 $ map parse $ lines co
    print ans
```

```
*Main> main1
Just 2480
```

少し時間はかかるがこれでも答えは求められた。

しかしパート2の数値は巨大である。デッキサイズ`base2`と繰り返し回数`reps`のいずれも46桁の2進数になっている。

```haskell
base2, reps :: Integer
base2 = 119315717514047 -- ‭110 1100|1000 0100|0101 1010|1111 0101|0110 0011|0011 1111‬
reps  = 101741582076661 -- ‭101 1100 1000 1000 1000 1110 1101 1011 1100 1010 1111 0101‬
```

このまま進むのではなく、より賢い方法が必要なのは明らかである。

実はこの問題のシャッフルテクニックは、デッキ枚数を法とする合同算術における線形関数になっている。
線形関数なので、f(x) = ax + b という形で表せて、また他の線形関数 g(x) = cx + d との関数合成が
g . f (x) = ac x + (bc + d) というまた別の線形関数になる。

パート2を見据えて、パート1からやり直してみよう。
線形関数 f(x) = ax + b を係数のタプル (a,b) で表すことにする。
ふたつの関数の合成と、引数の適用が定義できる。
```haskell
type LFunc = (Integer, Integer)

compose :: Integer -> LFunc -> LFunc -> LFunc
compose modulo (c,d) (a,b) = (a*c `mod` modulo, (b*c + d) `mod` modulo)

apply :: Integer -> LFunc -> Integer -> Integer
apply modulo (a,b) x = (a * x + b) `mod` modulo
```

各シャッフルのテクニックにより、カードがどのように並び変わるかという順方向の線形関数に翻訳しよう。

```haskell
forward :: Integer -> Shuffle -> LFunc
forward modulo  Deal     = (modulo-1, modulo-1)
forward modulo (CutN n)  = (1, modulo - n)
forward modulo (DealN n) = (n, 0)
```

テクニックのリストを関数合成して一つの線形関数にまとめる計算を抽出する。

```haskell
composes :: Integer -> [LFunc] -> LFunc
composes modulo = foldl1 (flip (compose modulo))
```

実行テストをするには、前方向の結果を問題文の形に並べなおす必要がある。

```haskell
forwardTest :: Integer -> [Shuffle] -> Deck
forwardTest modulo ss = map snd $ sort $ zip dests [0..]
  where
    lf = composes modulo $ map (forward modulo) ss
    dests = map (apply modulo lf) [0..modulo - 1]
```

```
*Main> forwardTest 10 sample1
[0,3,6,9,2,5,8,1,4,7]
*Main> forwardTest 10 sample2
[3,0,7,4,1,8,5,2,9,6]
*Main> forwardTest 10 sample3
[6,3,0,7,4,1,8,5,2,9]
*Main> forwardTest 10 sample4
[9,2,5,8,1,4,7,0,3,6]
```

これでパート1の答えは順方向の計算一発で求められる。

```haskell
main1a = do
    co <- readFile "input.txt"
    let ans = apply base1 (composes base1 $ map (forward base1 . parse) $ lines co) 2019
    print ans
```

```
*Main> main1a
2480
```

しかし実はこれでは、パート2の答えは出荷状態のデッキのカードをしらみつぶしに2020の位置に来たかどうかを計算するしかなくてつらい。
今作ったものとは逆の、テクニック後の位置からテクニック前の位置を求める向きの計算が必要である。
`Deal`と`CutN`はほぼ同様に線形関数を作れる。しかし`DealN`はどうするのだろうか。(1/n,0)としたいが整数にならない。
ここで、合同算術においてaの逆元を ax = 1 (mod p) となるxと定義して、
またそれが拡張ユークリッド互除法を用いて求められる、ということが調べるとわかる。

例えば数理物質科学研究科 数学専攻 坂井公氏による[計算機数学 I 講義ノート](http://www.math.tsukuba.ac.jp/~ksakai/compmath)の

- §4.7 法逆元計算（Modular inverse）に逆元の定義と求め方
- §4.3 拡張ユークリッド互除法 にアルゴリズム

がある。これを活用して`DealN`の逆変換を線形関数にする。

```haskell
-- ax + by = gcd a b を満たす整数 a b とgcd a bを求める
-- eea a b = (gcd a b, x, y)
eea :: Integer -> Integer -> (Integer, Integer, Integer)
eea a b = loop a 1 0 b 0 1
  where
    loop r0 s0 t0 0  _  _  = (r0, s0, t0)
    loop r0 s0 t0 r1 s1 t1 = let (q,r2) = divMod r0 r1 in loop r1 s1 t1 r2 (s0-q*s1) (t0-q*t1)
```

```haskell
backward :: Integer -> Shuffle -> LFunc
backward modulo  Deal     = (modulo-1, modulo-1)
backward modulo (CutN n)  = (1, n)
backward modulo (DealN n) = let (1,x,_) = eea n modulo in (x, 0)

backcomposes :: Integer -> [LFunc] -> LFunc
backcomposes modulo = foldl1 (compose modulo)

backwardTest :: Integer -> [Shuffle] -> Deck
backwardTest modulo ss = map (apply modulo lf) [0..modulo - 1]
  where
    lf = backcomposes modulo $ map (backward modulo) ss
```

```
*Main> backwardTest 10 sample1
[0,3,6,9,2,5,8,1,4,7]
*Main> backwardTest 10 sample2
[3,0,7,4,1,8,5,2,9,6]
*Main> backwardTest 10 sample3
[6,3,0,7,4,1,8,5,2,9]
*Main> backwardTest 10 sample4
[9,2,5,8,1,4,7,0,3,6]
```

これである位置の値がどこから来たかを一瞬で求められるようになった。
とはいえ`reps`回計算することは無理である。
ここで、この線形関数は関数合成を合成後の係数の対として実際に求めることができることを利用する。
具体的には、一回のプロセスの関数を`f[1]`としたとき、それを2回繰り返す関数は`f[1].f[1]=f[2]`、
4回繰り返す関数はf[2].f[2]=f[4]、…を求め、`reps`を2進数で表したときに`1`のある`f[n]`を合成することで
`f[reps]`を具体的に作る。これが作れたら`f[reps](2020)`が求める答えである。

線形関数におけるidentityの(1,0)を初期値として、次のようにしてこれは構築できる。

```haskell
buildf :: LFunc -> Integer -> LFunc -> LFunc
buildf g 0 _ = g
buildf g r f = buildf g1 (r `div` 2) (compose base2 f f)
  where
    g1 = if even r then g else (compose base2 g f)
```

動かそう。

```haskell
main2 = do
    co <- readFile "input.txt"
    let f = foldr1 (compose base2) $ map (inverse base2 . parse) $ lines co
    let fr = buildf (1,0) reps f
    let ans = apply base2 fr 2020
    print ans
```

```
*Main> main2
62416301438548
```
