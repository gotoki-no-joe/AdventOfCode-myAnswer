# 入力

数値列の列、をリストにして渡すことにする。

```haskell
runner i f = do
  xss <- map (map read . words) . lines <$> readFile i
  print $ f xss

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part1 :: [[Int]] -> Int
part1 xss = ...
```

# パート1

隣接する要素の差をとったとき、全て 1 以上 3 以下である（上昇列）か、
全て -1 以下 -3 以上である（下降列）であるときが安全の条件になる。
ひとつの列に対して、この条件を満たすか判定する述語を作る。

```haskell
prop1 :: [Int] -> Bool
prop1 xs = all (1 <=) ds && all (3 >=) ds || all (-3 <=) ds && all (-1 >=) ds
  where
    ds = zipWith (-) xs $ tail xs
```

これを満たす列の個数を数える。

```haskell
part1 = length . filter prop1
```

## 少しこすいやり方

全ての要素を真面目に調べる代わりに、差の最小値と最大値だけ見比べる、でもできる。
が、最大値と最小値を取り出すのに同じ計算量がかかるので、オーダーは変わらない。
ちなみに、最小値と最大値を同時に求める計算を使えば計算量は半分にはできる。

```haskell
prop1 :: [Int] -> Bool
prop1 xs = -3 <= dmin && dmax <= -1 || 1 <= dmin && dmax <= 3
  where
    (dmin, dmax) = minmaximum $ zipWith (-) xs $ tail xs

minmaximum :: Ord a => [a] -> (a, a)
minmaximum (x:xs) = loop x x xs
  where
    loop l u [] = (l,u)
    loop l u (x:xs)
      | x < l = loop x u xs
      | u < x = loop l x xs
      | True  = loop l u xs
```

# パート2

ひとつひとつの列は大した長さではないので、総当たりでいく。
リストから、いずれかひとつの要素を取り除いた列全て、をがんばって再帰関数で作ってもいいが、
そうしなくてもいい。

```haskell
import Data.List

drop1 xs = zipWith (++) (inits xs) (tail $ tails xs)

prop2 :: [Int] -> Bool
prop2 xs = any prop1 $ xs : drop1 xs

part2 :: [[Int]] -> Int
part2 = length . filter prop2

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2
```
