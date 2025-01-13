# パート1

自分のパズル入力が 36,000,000 だったので、それを前提に話を進める。

家番号 \\(N\\) が \\(a\\) の倍数であるとき、プレゼントを \\(10a\\) 受け取る、
その総和が目標を超えるような最小の \\(N\\) を見つけたい。
家番号 \\(N\\) に、小人 \\(N\\) は必ず訪問するので、
家番号 3,600,000 は小人 3,600,000 から 36,000,000 を受け取る。
よって \\(N \leq 3,600,000\\) 以下を調べればよい。

```haskell
import Data.Array.Unboxed

theInput :: Int
theInput = 36000000
theInput10 = div theInput 10

part1 = head $ filter ((theInput10 <=) . snd) $ assocs arr
  where
    arr :: UArray Int Int
    arr = accumArray (+) 1 (1,theInput10)
          [(j, i) | i <- [2 .. theInput10], j <- [i, i+i .. theInput10]]
```

探索範囲を倍々に増やしながら答えを見つけるまで続けるようなコードにすることもできる。

```haskell
part1a = loop 1 100
  where
    loop lb ub
      | null ans  = loop (succ ub) (min theInput10 (ub * 2))
      | otherwise = head ans
      where
        arr :: UArray Int Int
        arr = accumArray (+) 1 (lb,ub)
          [(j, i) | i <- [2..ub], let j0 = i * divrup lb i, j <- [j0, j0+i .. ub]]
        ans = filter ((theInput10 <=) . snd) $ assocs arr

-- 切り上げ除算
divrup x y = negate $ div (negate x) y
```

こちらの方が少し速く完了する。

# パート2

家番号 \\(K\\) に訪れる小人の番号は \\(K\\) 以下である。
\\(K=1\\) から順に、小人 \\(K\\) の配達まで済ませた状況を作り、家 \\(K\\) の個数を確認する。
目標に達していなければさらに次に進む。このとき、\\(K\\) 以下の家の状況は今後更新されないので捨ててよい。

```haskell
import qualified Data.IntMap as IM

part2 = loop 1 IM.empty
  where
    loop k im
      | n >= theInput = (k, n)
      | otherwise     = loop (succ k) im1
      where
        ((k1, n), im1) =
          IM.deleteFindMin $
          IM.unionWith (+) im $
          IM.fromDistinctAscList $
          [(i, 11 * k) | i <- map (k *) [1 .. 50]]
```
