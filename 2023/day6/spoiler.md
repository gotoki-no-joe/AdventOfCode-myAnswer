# 入力

2行しかない。ヘッダを取り除き、数値列にする。

```haskell
runner i f = do
  (ts:ds:_) <- map (map read . tail . words) . lines <$> readFile i
  print $ f ts ds

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int] -> Int
part1 ts ds =
```

# パート1

タイムは大した範囲ではないので総当たりして数える。

持ち時間 $T$ ミリ秒距離 $D$ ミリのレースで、
$0 \leq t \leq T$ 秒チャージしたとき、走行距離は $t (T - t)$ で、
記録を破るには $t (T - t) > D$ である必要がある。

```haskell
part1 :: [Int] -> [Int] -> Int
part1 ts ds = product $ zipWith f ts ds
  where
    f tee dee = length [() | t <- [0 .. tee], t * (tee - t) > dee]
```

# パート2

ファイルの読み込み方法を今更直すのも面倒なので、`show`して繋いで`read`しよう…

## bruitforce

さて、5日めがあれだった割に、連結しても8桁の数なら、力業でも答えを出すことはできるだろう。

```haskell
part2 ts ds = length [() | t <- [0 .. tee], t * (tee - t) > dee]
  where
    tee = read $ concatMap show ts
    dee = read $ concatMap show ds
```

## 二次方程式の解

それではあまりにも詰まらないので、もう少し考えてみると、
$t (T - t) > D$ を満たす $t$ の範囲を求めよとは、
上に凸な二次関数 $f(x) = -t^2 +Tt - D$ の、$f(x) > 0$ な、X軸より上になっている区間を聞かれているのと等しい。
$f(x) = 0$ を解いて
$$0 < \frac{T - \sqrt{T^2 - 4D}}{2} < x < \frac{T + \sqrt{T^2 - 4D}}{2}$$

この範囲の非負整数の個数は

$$\left \lfloor \frac{T + \sqrt{T^2 - 4D}}{2} \right \rfloor - \left \lceil \frac{T - \sqrt{T^2 - 4D}}{2} \right \rceil + 1$$

ということで、これを直接計算しよう。

```haskell
part2a :: [Int] -> [Int] -> Int
part2a ts ds = f1 - f2 + 1
  where
    t = read $ concatMap show ts
    d = read $ concatMap show ds
    sq = sqrt (t * t - 4 * d)
    f1 = floor   ((t + sq) / 2)
    f2 = ceiling ((t - sq) / 2)
```

AtCoderだとここで、浮動小数点演算の計算誤差が影響するような入力が来るので、$f(0) = -D < 0, f(T/2) > 0, f(T) = -D < 0$ を利用して二分探索で区間を求める必要があるだろうな、とか。
