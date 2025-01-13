# 入力

マイナスと数字以外を無視して数値列だけ読み込む。

```haskell
import Data.List.Split

parse :: String -> [Int]
parse = map read . wordsBy (flip notElem ('-':['0'..'9']))

runner i f = readFile i >>= print . f . map parse . lines
```

# パート1

材料が N 種類あり、全部で小さじ W 杯を使うようなやり方で、
それぞれをどれだけ使うかのというレシピを配合のリストで表し、レシピのリストを作る。

```haskell
recipe :: Int -- 残りの材料種類
       -> Int -- 残りの配合量
       -> [[Int]] -- レシピのリスト
-- 材料が最後なら残量全部それ
recipe 1 w = [[w]]
-- 配合がいっぱいなら、残りの材料は全部使わない
recipe k 0 = [replicate k 0]
-- 先頭の材料をx = 0～w使い、残りの材料の使い方は再帰的に求めて組み合わせる
recipe k w = [x:ys | x <- [0..w], ys <- recipe (pred k) (w - x)]
```

重複した呼び出しが多いので、配列DPで計算量を節約する。

```haskell
import Data.Array

mkRecipe k w = recipeA ! (k, w)
  where
    bnds = ((1,0), (k,w))
    recipeA = listArray bnds $ map recipeF $ range bnds
    recipeF (1, w) = [[w]]
    recipeF (k, 0) = [replicate k 0]
    recipeF (k, w) = [x : ys | x <- [0..w], ys <- recipeA ! (pred k, w - x)]
```

レシピの全てのやり方について、材料ごとの特性値を掛けて足し合わせ、
（カロリーを除いて）0で足切りした総積を求め、その最大値を見つける。

```haskell
import Data.List

part1 list = maximum
    [ product $ init vals
    | ws <- mkRecipe (length list) 100
    , let vals = values ws ]
  where
    tlist = transpose list
    values ws = map (max 0 . sum . zipWith (*) ws) tlist

test1 = runner "test.txt" part1
main1 = runner "input.txt" part1
```

# パート2

カロリーが500のものだけ残し、その中でスコアの最高点を求める。

```haskell
part2 list = maximum
    [ product $ init vals
    | ws <- mkRecipe (length list) 100
    , let vals = values ws
    , last vals == 500 ]        -- ココ
  where
    tlist = transpose list
    values ws = map (max 0 . sum . zipWith (*) ws) tlist
```
