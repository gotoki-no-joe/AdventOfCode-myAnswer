# 入力

コンピュータの名前の文字列のペア、でなく長さ2のリストで一行を表現して、そのリストにする。
タプルよりむしろ扱いが楽かもしれない。

`input.txt` も覗いて、2文字-2文字固定と割りきってしまってもいいかも。
一応、任意の文字列としておく。

```haskell
runner i f = do
  pqs <- map parse . lines <$> readFile i
  print $ f pqs

parse l = [as, bs]
  where
    (as,_:bs) = break ('-' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 pqs = ...
```

# パート1

コンピュータを頂点、接続を辺とする無向グラフを考える。
頂点をキーにしたマップに、接続している頂点の集合を付けてグラフの隣接行列的な情報を集約する。
接続は無向なためこの行列は対角線について対称になるが、
キーよりも大きい相手についてだけ登録する、上三角行列に相当する形にしておく。

```haskell
import qualified Data.Map as M
import qualified Data.Set as S

part1 pqs = ...
  where
    m = fmap S.fromList $ M.fromListWith (++) $ [(minimum pq,[maximum pq]) | pq <- pqs]
```

このマップ `m` があれば、相互に接続された3頂点は、

- いずれかの頂点 `p` を一つ選び、（`m` から得られる）その隣接頂点集合を `qS` としたとき、
- `qS` のいずれかの要素 `q` を一つ選び、その隣接頂点集合を`rS` としたとき、
- `qS` と `rS` の共通部分集合の要素 `r` について
- `(p,q,r)` は相互に接続されている

として全てが重複なく取り出せる。
これらのいずれかの1文字めが `t` であることを確認し、その個数を数える。

```haskell

part1 pqs = length
  [ (p,q,r)
  | (p,qS) <- M.assocs m
  , q <- S.elems qS
  , M.member q m -- q, r は m にキーとして現れない可能性がある
  , r <- S.elems $ S.intersection qS $ m M.! q
  , elem 't' $ map head [p,q,r]
  ]
  where
    m = fmap S.fromList $ M.fromListWith (++) $ [(minimum pq,[maximum pq]) | pq <- pqs]
```

`length` を外せば、問題文の例と一致するか確認できる。

# パート2

部分グラフで、完全グラフになっているようなものの最大のものを発見せよと。
こういうのは「最大クリーク」とい、NP困難なのだそうな。

とりあえず、グラフの情報はパート1と同様な形に、ただし、その後の計算を高速化するために、
まず頂点に1始まりで背番号を振り、整数で扱うことにする。
また、その後の都合で、接続関係は頂点番号がより小さいものについて集めておくことにする。

```haskell
import qualified Data.IntSet as IS

import Data.Array.IArray

part2 pqs = ...
  where
    pS = S.fromList $ "" : concat pqs
    n = pred $ S.size pS
    g :: Array Int IS.IntSet
    g = amap IS.fromList $ accumArray (flip (:)) [] (1, n)
        [maxmin ij | pq <- pqs, let ij = map (flip S.findIndex pS) pq]

    maxmin (a:b:_)
      | a >= b    = (a,b)
      | otherwise = (b,a)
```

深さ優先探索で全ての完全グラフであるような部分集合を列挙することを考える。

ある段階まで調べ終わった状態は、選択した頂点集合と、それら全てと接続している頂点の集合の対で表現される。
初期値はそれぞれ空集合と全体集合の対である。
さらに、頂点番号を降順に調べていくとして、次はどこを調べるか、も持たせておく。

- 今から調べる頂点番号 `k`
- 選択している頂点集合 `mem`
- `mem` の全てと接続している頂点の集合 `con`

に対して、`k` を採用した状態と不採用にした状態の二つを探索する必要がある。

`k` を採用した状態
- `mem` に `k` を追加
- `con` は `k` の辺と共通部分に絞る `con1` とする
- 次の `k` は `con1` の最大の要素　これを探す計算を補助するために、頂点番号0を番兵として配置する

`k` を採用しなかった状態
- `mem` `con` は変化なし
- 次に調べる頂点は、`con` の `k` 未満の最大値

再帰で `k=0` まで降りたとき、再帰呼び出しを止め、結果を一つ返す。
二つの分岐に枝分かれした両方の結果の大きい方を選んで返せば答えになる。

```haskell
part2 pqs = post ans1
  where
    post = intercalate "," . map (flip S.elemAt pS) . IS.elems

    g = amap (IS.fromList . (0 :)) $ accumArray (flip (:)) [] (1, n) -- 番兵入り
        [maxmin ij | pq <- pqs, let ij = map (flip S.findIndex pS) pq]

    ans1 = snd $ dfs n (0, IS.empty) (IS.fromDistinctAscList [0 .. n])
      where
        dfs :: Int -- 今から調べる頂点番号
            -> (Int, IS.IntSet) -- 今調査している状態、サイズと頂点集合
            -> IS.IntSet        -- と、次に選べる、全結合になっている頂点集合
            -> (Int, IS.IntSet) -- 答え、サイズと頂点集合
        dfs 0 ent _ = ent                       -- 底まで降りたら答えを返す
        dfs k ent@(sz, mem) con = max res1 res2 -- 分岐して戻ってきた値の大きい方
          where
            sz1  = succ sz
            mem1 = IS.insert k mem
            con1 = IS.intersection con $ g ! k
            res1 = dfs (IS.findMax con1) (sz1, mem1) con1     -- kを入れて再帰
            res2 = dfs (fromJust $ IS.lookupLT k con) ent con -- kを入れずに再帰
```

これでもう十分な結果が得られたが、もっと枝刈りをすることを考えよう。
これまでに見つかっている（作りかけのものも含めて）最大の結果を追加の引数で持ち回すことにする。

上の `dfs` の定義にそれを単純に盛り込むと、概略として

```haskell
dfs 0 = max arg best
dfs k arg best = max (dfs k' arg1 best) (dfs k' arg2 best)
```

このような感じになる。
このようなやり方で再帰を二手に分けると、片方の分岐の最新の結果をもう片方の分岐が参照できない。
そこで構造を少し変えてやる。

```haskell
dfs 0 = max arg best
dfs k arg best = dfs k' arg1 $ dfs k' arg2 best
```

これで、最新の結果 `best` が、`dfs` の適用の連鎖を駆け上がる形で最新情報を送り込める。

ここで、`best` の値は `k=0` を待たずに、再帰の途中でも可能なら更新をかけるものとする。

さて、そのような現時点での最適解があると、今調べている事例が手遅れ、という判定ができる。
つまり、今 `k` を加えるとき、1～k全てのk頂点を追加できた最善の場合でも、
最適解を更新できないとわかったら、この事例をそれ以上計算する意味はない。なので放棄する。

以上の改善を盛り込む。

```haskell
    ans2 = snd $ dfs n (0, IS.empty) (IS.fromDistinctAscList [0 .. n]) (0, IS.empty)
      where
        dfs :: Int -- 今から調べる頂点番号
            -> (Int, IS.IntSet) -- 今調査している状態、サイズと頂点集合
            -> IS.IntSet        -- と、次に選べる、全結合になっている頂点集合
            -> (Int, IS.IntSet) -- 現時点での最適解
            -> (Int, IS.IntSet) -- 答え、サイズと頂点集合
        dfs 0 _ent _ best = best               -- bestは常時更新するので、_entがベスト解ならbestも同じ内容
        dfs k ent@(sz, mem) con best@(bestsz,_)
          | k + sz < bestsz = best                       -- 挽回不能な差がついている
          | otherwise =
            dfs (fromJust $ IS.lookupLT k con) ent con $ -- kを入れずに再帰
            dfs (IS.findMax con1) ent1 con1 $            -- kを入れて再帰
            if sz == bestsz then ent1 else best          -- sz==bestszならsucc sz>bestszなので更新する
          where
            mem1 = IS.insert k mem
            con1 = IS.intersection con $ g ! k
            ent1 = (succ sz, mem1)
```
