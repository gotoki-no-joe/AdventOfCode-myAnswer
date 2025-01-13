# 入力

行を、二つの都市名と距離のタプルにする。

```haskell
runner i f = readFile i >>= print . f . map parse . lines

parse :: String -> (String,String,Int)
parse xs = (ws !! 0, ws !! 2, read $ ws !! 4)
  where
    ws = words xs
```

# パート1

都市間を飛び回る距離なので、どちら向きにも移動できる無向グラフと解釈する。
内容は巡回セールスマン問題に他ならないが、都市の数が大したことないので、
都市のリストの順列を生成し、その距離を求める。

行 `(c1,c2,dist)` に対して、`Map` に二つのキー `(c1,c2)`, `(c2, c1)` いずれも値は `dist` を入れる。

```haskell
import qualified Data.Map as M

part1 ccds = ...
  where
    distMap :: M.Map (String,String) Int
    distMap = M.fromList $ concat [[((c1, c2), d), ((c2, c1), d)] | (c1,c2,d) <- ccds]
```

この表があれば、都市名のリストを順路と見なして、その総距離を求められる。

```haskell
    fullDist :: [String] -> Int
    fullDist cs = sum $ map (distMap M.!) $ zip cs (tail cs)
```

都市名の一覧を作る。

```haskell
import Data.List

    cities = nub $ concat [[c1, c2] | (c1,c2,_) <- ccds]
```

これの全ての順列に対する距離の最小値が答え。

```haskell
part1 ccds = minimum $ map fullDist $ permutations cities
  where
    ...
```

# パート2

最大値と最小値は同時に求めると効率がよい。

```haskell
part12 ccds = minmaximum $ map fullDist $ permutations cities
  where
    ...

minmaximum :: Ord a => [a] -> (a,a)
minmaximum (x:xs) = foldl' step (x,x) xs
  where
    step lbub@(lb,ub) x
      | x < lb = (x, ub)
      | ub < x = (lb, x)
      | otherwise = lbub
```

