# 入力

コンピュータの名前の文字列のペア、でなくリストで一行を表現して、そのリストにする。
タプルよりむしろ扱いが楽かもしれない。

`input.txt` も除いて、2文字2文字固定と割りきってしまってもいいかも。
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
- `qS` のいずれかの要素 `q` を一つ選び、その隣接著点集合を`rS` としたとき、
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

ググっても珍妙な命令的アルゴリズムばかりひっかかる。

とりあえず、グラフの情報はパート1と同様な形に、ただし、その後の計算を高速化するために、
まず頂点に背番号を振り、整数で扱うことにする。

```haskell
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Array.IArray

part2 pqs = ...
  where
    pS = S.fromList $ concat pqs
    ub = pred $ S.size pS
    g :: Array Int IS.IntSet
    g = amap IS.fromList $ accumArray (flip (:)) [] (0, ub)
        [(minimum ij, maximum ij) | pq <- pqs, let ij = map (flip S.findIndex pS) pq]
```

頂点を先頭から順に検討していくループを回す。

前回のステップまでに、そこまでに検討した頂点だけ使って作ることができる
完全グラフであるような頂点集合を全て集めて袋に入れておく。
ここには候補が入っていることになる。

次に番号 $k$ の頂点を考えるとき、
- 袋の中から、全ての所属する頂点が $k$ と接続しているような候補を見つける
- それらに $k$ を追加した新たな候補を袋に追加する
とする。

前半をするために、全ての所属する頂点の隣接頂点集合の共通部分、を持っておくと話が早い。
袋は単なるリストで扱ってもよいが、所属する頂点の個数を優先度とした優先度付きキューで表すと、
全て終わって最大のものを取り出す計算が $O(1)$ でできる他、
優先度の低いものが遅延評価で後回しにされる様子が、効果的な枝刈りになるのではないかと欲張ってみる。

```haskell
import qualified Data.Heap as H
import Data.List

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 pqs = post ans
  where
-- 袋には初め、空リストがひとつ入っている
    initial = H.singleton $ H.Entry 0 (IS.empty, IS.fromDistinctAscList [0 .. ub])
    H.Entry _ (ans, _) = H.minimum $ foldl step initial [0 .. ub]
    step cands k = H.union cands $ H.fromList
        [ H.Entry (pred sz) (IS.insert k mem, IS.intersection con $ g ! k) -- kを入れたものを袋に追加
        | H.Entry sz (mem, con) <- H.toUnsortedList cands   -- 候補の中で
        , IS.member k con                                   -- 今回の k を入れても完全グラフなもの
        ]
    post = intercalate "," . map (flip S.elemAt pS) . IS.elems
```

テスト実行。

```
ghci> test2
"co,de,ka,ta"
```

コードカタ、と。ギークだ。

## もっと速く？

上では「袋」をひとつで考えた。
そして毎回、注目する $k$ を入れられるものを調べる、とするのは、袋の要素が増えると計算が重くなる。

$k$ を追加した結果、その次に加入させることのできる最小のノード番号はその場で得られる。
バケツソートのように袋を $N+1$ 枚用意しておき、次に検討する最小のノード番号の袋に入れてしまえば、
いちいち無駄な比較をしなくてよくなる。
ただしこのとき、$k$ を追加しなかった方について、前はひとつの袋に入れたままでよかったが、
$k$ を削除した次の検討候補の袋にそれぞれ配る手間が増える。

$N+1$ の $+1$ は、最終的な答えを集める場所。

なんにせよ、激しく書き換わる$N+1$個の袋の列は、mutable array で表現する他ないだろう。

ちょっと長くなった。
袋自体は上とおなじ優先度付きキューで表している。
`fromList`も`toUnorderedList`も $O(N)$ なのでリストでしてもオーダーは同じ。

```haskell
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Maybe

-- ヒープのバケツを使う
    n = S.size pS
    g1 :: Array Int IS.IntSet
    g1 = amap (IS.insert n) g -- 番兵を潜ませる
    ans1 = runST $ do
      bags <- newArray (0, n) H.empty :: ST s (STArray s Int (H.Heap (H.Entry Int (IS.IntSet, IS.IntSet))))
      insert2B bags 0 (H.Entry 0 (IS.empty, IS.fromDistinctAscList [0 .. n]))
      forM_ [0 .. ub] (\k -> do
        cands <- readArray bags k
        writeArray bags k H.empty -- メモリに配慮
        forM_ (H.toUnsortedList cands) (\ent@(H.Entry sz (mem, con)) -> do
          let con1 = IS.intersection con $ g1 ! k
          let ent1 = H.Entry (pred sz) (IS.insert k mem, con1)
          insert2B bags (IS.findMin con1) ent1
          insert2B bags (fromJust $ IS.lookupGT k con) ent
          )
        )
      H.Entry _ (ans, _) <- H.minimum <$> readArray bags n
      return ans

-- i番目の袋のヒープにentを追加する
    insert2B bags i ent = do
      cands <- readArray bags i
      writeArray bags i $ H.insert ent cands
```

すごく速くなった。
完全グラフであるような部分グラフを全て求める手順はこれで完成。

## 枝を刈れ

最大クリークだけを発見するアルゴリズムなら、さらに枝刈りができる。
要素 $k$ を考えるとき、そこにある最大の候補に残る頂点 $N-k$ 個全てを追加できたとしても
現状のベストを下回るならこの袋の内容を全て諦めるとか、要素のサイズを個々に考慮して打ち切りを判定するとか。

袋を優先度付きキューからリストに簡単化し、要素 $k$ について一括枝刈り、ではなく
個々の候補ごとに打ち切りを判定するだけにして書いてみた。
また、最終結果を集める特別なバケツ $N$ 番だけ、挿入するものをベストスコアのものに絞ってみた。

```haskell
-- リストのバケツを使い、現在のベストスコアを追跡することで、見込みのない候補をスターリンする
-- バケツのNに入れるときは、ベストスコアのものだけ入れる
    ans3 = runST $ do
      bags <- newArray (0, n) [] :: ST s (STArray s Int [(Int, IS.IntSet, IS.IntSet)])
      writeArray bags 0 [(0, IS.empty, IS.fromDistinctAscList [0 .. n])]
      bestsizeN <- foldM (\bs k -> do
        cands <- readArray bags k
        writeArray bags k [] -- メモリに配慮
        let szmax = maximum $ map getsz cands -- 今回処理する候補のサイズの最大値
        let bs1 = max bs $ succ szmax         -- 更新されるベストスコア
        let cands1 = filter (\e -> bs1 <= n - k + getsz e) cands -- 意味のある候補だけ残す
        forM_ cands1 (\ent@(sz, mem, con) -> do
          let con1 = IS.intersection con $ g1 ! k
          let ent1 = (succ sz, IS.insert k mem, con1)
          insert2La bs1 bags (IS.findMin con1) ent1
          insert2La bs1 bags (fromJust $ IS.lookupGT k con) ent
          )
        return bs1
        ) 0 [0 .. ub]
      (_,ans3,_) <- head . filter ((bestsizeN ==) . getsz) <$> readArray bags n
      return ans3

    getsz (sz,_,_) = sz

-- 袋のi番目のヒープにentを追加する
-- i==Nのとき、サイズがbs未満なら無視する
    insert2La bs bags i ent
      | i == n, getsz ent < bs = return ()
      | otherwise = do
          cands <- readArray bags i
          writeArray bags i (ent : cands)
```

いい感じのDPになった気がするのだけどどうだろう。
