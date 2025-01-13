# パート1

座標がどちらに広がるのか事前に予測できないので、配列で扱うのは難しい。
座標の集合で扱うと制約なく扱いやすい。

```haskell
import qualified Data.Set as S

part1 :: String  -- 指示
      -> Int     -- 答え
part1 = S.size . S.fromList . scanl step (0,0)

step (x,y) '^' = (pred x, y)
step (x,y) 'v' = (succ x, y)
step (x,y) '<' = (x, pred y)
step (x,y) '>' = (x, succ y)
```

# パート2

入力を、サンタ用とメカサンタ用に分離した後はパート1と同じやり方で座標を得て、
合わせてから数えればよい。

```haskell
part2 :: String  -- 指示
      -> Int     -- 答え
part2 cs = S.size $ S.union ps1 ps2
  where
    ps1 = go [c | (c, 0) <- zip cs $ cycle [0, 1]]
    ps2 = go [c | (c, 1) <- zip cs $ cycle [0, 1]]

go = S.fromList . scanl step (0,0)
```

交互に取り出すには他にも、

```haskell
import Data.List (transpose)
import Data.List.Split (chunksOf)

part2 cs = S.size $ S.unions $ map go $ transpose $ chunksOf 2 cs
```

のようにしてもできる。
