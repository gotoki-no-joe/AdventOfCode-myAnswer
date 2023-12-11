{-
sample

Time:      7  15   30
Distance:  9  40  200

input

Time:        51     92     68     90
Distance:   222   2031   1126   1225

こういうフォーマットは、structured data とは呼ばないよねぇ？

part1
timeも小さいし、問題文にあるように、総当たりで数えてみるだけでいいだろう。
-}

-- t msのレースで可能な結果の一覧を作る
-- ボタン0～tまで押して加速して、残りの時間を走行した結果
records :: Int -> [Int]
records t = [ s * (t - s) | s <- [0..t]]

part1 tds = product [length $ filter (d <) $ records t | (t,d) <- tds]

sample :: [(Int, Int)]
sample = [(7,9), (15,40), (30, 200)]

input :: [(Int,Int)]
input = [(51,222),(92,2031),(68,1126),(90,1225)]

{-
パート2
すごい理由で桁数をいきなり上げてきたな。
-}

p2s = length $ filter (940200 <) $ records 71530
p2i = length $ filter (222203111261225 <) $ records 51926890

{-
でも、ブン回しで計算できちゃったぞ。

ghci> p2s
71503
ghci> 222203111261225 :: Int
222203111261225
ghci> p2i
42515755

ちょっとダサいから、クールな解き方を考えようか。
これが通るなら、day5もブン回しでよかったのだし。


[i * (t - i) | i <- [0 .. t]]
=
zipWith (*) [0..t] (reverse [0..t])

これは図形的には、2辺の長さがtになる長方形で、面積がdを超える場合の数を探している。
j * (t - j) > d
j^2 - jt + d < 0 という整数二次不等式を考える。

j = 0 のとき LHS = d > 0
j = t のとき LHS = t^2 - t^2 + d = d > 0 なので確実に領域外
j = t/2 のとき LHS = t^2/4 - t^2/2 + d = d - t^2/4 が最小値で、これが >= 0 なら解なしだが、それはないとすると、
ここは確実に LHS < 0 で不等式を満たすパラメータ。

そこで、unsat=0, sat=t/2 で二分探索して下限を、unsat=t,sat=t/2 で二分探索して上限を探し、その間が全て解に含まれる、
とすれば、整数計算だけでもプログラムで答えが出せるが、
しかし、実数で強引に、j^2 - jt + d < 0 という二次不等式の2解を見つけるならば、解の公式から
a,b = (t ± √(t^2 - 4d)) / 2
について a <= j <= b が解なので、整数解は
ceiling a <= j <= floor b
となる。
-}

realv2 :: Double -> Double -> Int
realv2 t d = floor ((t + dee) / 2) - ceiling ((t - dee) / 2) + 1
  where
    dee = sqrt $ t * t - 4 * d

realv2s = realv2 71530 940200
realv2i = realv2 51926890 222203111261225

{-
ghci> realv2s
71503
ghci> realv2i
42515755

「二次方程式の解を計算するだけ」問題、思いっきりでちゃいましたねぇ。
-}
