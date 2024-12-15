# 入力

今回、数字文字に加えてマイナス記号を残せば、必要な情報が残る。
また、ステージの広さが別パラメータになっているので、外から渡す。

```haskell
import Data.Char
import Data.List.Split

runner i f = readFile i >>= print . f. map (map read . wordsBy (not . dm)) . lines

dm '-' = True
dm c = isDigit c

test1 = runner "sample.txt" (part1 11 7)

main1 = runner "input.txt" (part1 101 103)

part1 :: Int -> Int -> [[Int]] -> Int
part1 wide tall robos = ...
  where
```

# パート1

座標は初めから 0～wide-1, 0～tall-1 の範囲で与えられているので、
剰余をとれば指定の時刻の位置が求められる。
全員の座標を一度に求める計算を括りだしておく。

```haskell
roboPos :: Int -> Int -> Int -> [[Int]] -> [(Int, Int)]
roboPos wide tall t nss =
  [ (mod (x + t * vx) wide, mod (y + t * vy) tall)
  | [x,y,vx,vy] <- nss
  ]
````

分類によって集計するのは `Data.Array.accumArray` の出番。
そして象限の値を掛け合わせれば終わり。

```haskell
import Data.Array

part1 :: Int -> Int -> [[Int]] -> Int
part1 wide tall nss = product $ map (cnt !) [(LT,LT),(LT,GT),(GT,LT),(GT,GT)]
  where
    cnt = accumArray (+) 0 ((LT,LT),(GT,GT))
          [ ((compare x100 ox, compare y100 oy),1)
          | (x100, y100) <- roboPos wide tall 100 nss]
    ox = div wide 2
    oy = div tall 2
```

# パート2

全然使用として成り立ってない問題の要求…つら…

ロボットはワープして動き回っているので、全員が同時に出発点に戻る周期があるはず。

幅$W$として、ロボットのX座標について、時刻 $k$ で元に戻るとすると  
 $(x + k \cdot vx) \bmod W = x$  
$x + k \cdot vx = x \mod W$  
$k \cdot vx = 0 \mod W$  
任意の $vx$ についてこれが成り立つ $k$ とはつまり $k = W$  
Y座標についても同様なので、つまり周期は高さ $T$ として $\textrm{lcm}(W,T) = 10403$ となる。
画像で出力してひたすら眺めて探す、でも何とか行けそうな数字だが…

## 統計学

標準偏差とは、データの散らばり具合を数値化するものである。
標準偏差は分散の平方根で、分散は平均値からの差の二乗の平均値で定義される。
各時刻でのロボットの散らばり具合を分散で表現して、その小さい順に時刻をいくつかピックアップしてみよう。

特定の時刻の分散を求める関数を立てる。

```haskell
distrib wide tall nss t = ...
  where
    xys = roboPos wide tall t nss
```

まず平均値が必要。面倒なので整数のままでざっくり近似する。

```haskell
    n = length nss
    xave = div (sum $ map fst xys) n
    yave = div (sum $ map snd xys) n
```

平均値との差の二乗を求めるのだが、これも距離の二乗で近似する。

```haskell
distrib wide tall nss t = dist
  where
    ...
    dist = div (sum [(x - xave)^2 + (y - yave)^2 | (x,y) <- xys]) n
```

周期の全ての時刻について分散を求めて、
小さい方から10シーンを取り出す。

```haskell
main2D = runner "input.txt" (part2D 101 103)

part2D :: Int -> Int -> [[Int]] -> [(Int,Int)]
part2D wide tall nss = take 10 dts
  where
    dts = sort [(distrib wide tall nss t, t) | t <- [0 .. lcm wide tall]]
```

左が分散、右が時刻。抜きんでて先頭の分散が小さくなっているのは、多分そういうことだろう。
その時刻の様子をアスキーアートで取り出そう。

```haskell
main2I t = readFile "input.txt" >>= putStrLn . part2I 101 103 t . map (map read . wordsBy (not . dm)) . lines

part2I :: Int -> Int -> Int -> [[Int]] -> String
part2I wide tall t nss =
    intercalate "\n" $ transpose $ chunksOf tall $ elems arr
  where
    arr = accumArray (flip const) '.' ((0,0),(wide-1, tall-1))
          [(xy, '#') | xy <- roboPos wide tall t nss]
```
