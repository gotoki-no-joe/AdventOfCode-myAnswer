# 入力

標高の二次元配列を渡せばよいだろう。
数字文字 `Char` から整数 `Int` に直す必要すらないだろう。（なぜわかる？）

```haskell
import Data.Array

import qualified Data.Set as S

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Array (Int,Int) Char -> Int
part1 arr = ...
```

# パート1

命令型言語だと探索アルゴリズムの出番だが、遅延評価配列を使った集めるDPで簡単に求められる。

- 標高9のマスは、自分自身に到達できる
- 標高 $h$ のマスは、周囲のマスのうち、標高 $h+1$ のマスが到達できる頂上全てに到達できる

ここで、標高により、自分自身の内容を判定する相手が静的に定められることが重要で、
これが相互参照するような問題にはこの方法は使えない。

そのようなDPを行う計算を一般化する。

```haskell
-- 範囲bndsの位置iのマスに対して、n i で与えられる周囲のマス j について
-- jとDP結果のタプルのリストjdsから f i jds でDP結果を求める
dp :: Ix i => (i, i) -> (i -> [(i, b)] -> b) -> (i -> [i]) -> Array i b
dp bnds f n = dpArr
  where
    dpArr = listArray bnds [ f i [(j, dpArr ! j) | j <- n i] | i <- range bnds]
```

これを使えば全てのマスについて、そこから到達できる `9` のマスの一覧が得られる。
そして `0` のマスの集合の要素数の合計が答え。

```haskell
part1 :: Array (Int,Int) Char -> Int
part1 arr = sum [S.size s | ('0', s) <- zip (elems arr) (elems score)] -- 標高0のそれの個数の和
  where
    score = dp (bounds arr) gather (upways arr)   -- それぞれのマスから行ける9のマスの位置集合
    gather i jds                                  -- 集める計算
      | arr ! i == '9' = S.singleton i            -- 頂上は自分自身
      | otherwise      = S.unions $ map snd jds   -- 周囲の結果を統合

upways arr p@(i,j) =                                          -- 集める元のマスリスト
  [ q
  | q <- [(pred i, j), (succ i, j), (i, pred j), (i, succ j)] -- 上下左右に隣接して
  , inRange (bounds arr) q                                    -- はみ出していなくて
  , succ (arr ! p) == arr ! q]                                -- 標高が1大きい
```

# パート2

到達できる `9` への経路の本数を数えるものに `gather` を差し替えるだけ。

```haskell
test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Array (Int,Int) Char -> Int
part2 arr = sum [s | ('0', s) <- zip (elems arr) (elems score)] -- 標高0の経路数の和
  where
    score = dp (bounds arr) gather (upways arr) -- それぞれのマスから行ける9のマスへの経路数
    gather i jds                                -- 集める計算
      | arr ! i == '9' = 1                      -- 頂上は経路数1
      | otherwise      = sum $ map snd jds      -- 周囲の経路数の合計
```
