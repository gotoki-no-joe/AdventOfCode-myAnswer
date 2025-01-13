# 入力

フォーマット冗長さがすごい。
行を `words` した結果のうち、必要な情報を含む要素の位置を調べる。

|  位置  |  内容  |
| ---: | ---- |
|  0  |  主語  |
|  2  |  gain/lose  |
|  3  |  幸せ度 |
|  10  |  隣人  |

この結果を保存するデータ構造を選ぶのに、本体でどう使うかを考える。
人の名前の順列を作り、隣同士のペアで幸せ度を集計する。
つまり、人の名前の（主語, 隣人）という対をキーとすればよいだろう。

```haskell
parse :: String -> ((String,String), Int)
parse l = (pq, x)
  where
    ws = words l
    pq = (ws !! 0, init ws !! 10)
    x = (if ws !! 2 == "gain" then id else negate) (read $ ws !! 3)

runner i f = readFile i >>= print . f . map parse . lines
```

# パート1

最後は出発点に戻ってくるという条件の追加された巡回セールスマン問題に他ならない。
要素数も9日目と同じ8。

幸福度は、主語の側と隣人の側があるので足し合わせておく。

ペアの左を集めて `nub` することで参加者一覧を得て、順列を作り、幸福度の合計を求める。
先頭の一人は固定して構わない。

```haskell
import Data.List
import qualified Data.Map as M

part1 pqxs = maximum $ map score $ permutations ps
  where
    (p1:ps) = nub $ map (fst . fst) pqxs
    pqxm = M.fromListWith (+) $ concat [[pqx, ((q,p),x)] | pqx@((p,q),x) <- pqxs]
    score ps = sum [pqxm M.! (p,q) | (p,q) <- zip (p1 : ps) (ps ++ [p1])]

test1 = runner "test.txt" part1
main1 = runner "input.txt" part1
```

# パート2

書いてあるとおりに、自分を表に幸福度0で追加し、今度は自分を先頭にして計算してもよいが、
自分に関する幸福度は0なので、足しても足さなくても同じ。
つまり、長いテーブルに横並びに座った順列の幸福度を考えればよい。

```haskell
part2 pqxs = maximum $ map score $ permutations ps
  where
    ps = nub $ map (fst . fst) pqxs
    pqxm = M.fromListWith (+) $ concat [[pqx, ((q,p),x)] | pqx@((p,q),x) <- pqxs]
    score (p1:ps) = sum [pqxm M.! (p,q) | (p,q) <- zip (p1:ps) ps]

main2 = runner "input.txt" part2
```
