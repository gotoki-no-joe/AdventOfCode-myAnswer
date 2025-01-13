# 入力

3文字固定っぽいけど、とりあえずそのまま読み込む。

```haskell
import Data.List

runner i f = do
  ls <- map parse . lines <$> readFile i
  print $ f ls

parse :: String -> [String]
parse l = as : words bs
  where
    (as,_:_:bs) = break (':' ==) l

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 ls = ...
```

# パート1

## 下準備

無向グラフから3辺を切ることで、連結していたグラフが2つに分かれるその3辺を見つけることが目的。
扱いやすさのため、まずは頂点に背番号を割り振る。
コロンの左に登場しないものもあるので、とにかく全て集める。

```haskell
import qualified Data.Set as S

part1 ls = ...
  where
    nodes = S.fromList $ concat ls
    n = pred $ S.size nodes
```

`succ . flip S.findIndex nodes` により、ノードの名前から1始まりの背番号を求められる。
これを使って、辺の情報を整数のペアの形にする。
また、この順に辺にも背番号を付け、番号から両端ペアを取り出せるようにしておく。

```haskell
import Data.Array

    ijs = [(i,j) | l <- ls, let i:js = map (succ . flip S.findIndex nodes) l, j <- js]
    m = length ijs
    edges = listArray (1,m) ijs
```

頂点番号をキーに、隣接する頂点番号とその辺の番号のペアのリストを値にもつ配列で、グラフを表現する。
全ての辺を両向きに張った、完全版のグラフを作っておく。

```haskell
type Graph = Array Int [(Int,Int)] -- 頂点番号、隣接頂点の番号と辺の番号

    graph0 :: Graph
    graph0 = accumArray (flip (:)) [] (1,n) $ concat [[(i,(j,a)),(j,(i,a))] | (a, (i,j)) <- assocs edges]
```

## 本編

切り離すことでグラフが分割される3本の辺をどう見つけるか。
1本なら、Union-Findを用いてグラフの連結を調べられるので、いずれかの辺を除くことを総当たりで計算すればできるが、
本番データでは辺が3000からあるので、${}_{3000}C_3 = 4,495,501,000$ 通りを力任せにするのは無謀。

次のような手順を思いついた。

- いずれかの辺aを任意に選ぶ。（これが偶然、3辺の1つと想像しよう。）aの両端をi,jとする。
aを除いたグラフg1を作り、g1におけるiからjへの最短経路p1を探す。これは3辺のうちの残りのいずれかを通過するはず。
- p1に含まれる辺b、ただし背番号はaより大きいものを任意に選ぶ。（これが偶然、3辺のもう一つと想像しよう。）
bの両端をk,lとする。g1からさらにbも除いたグラフg2を作り、
g2におけるiからjへの最短経路p2と、kからlへの最短経路p3を探す。
これらは3辺のうち残りの1辺を通過するはず。
- p2とp3に共通して含まれる辺c、ただし背番号はaより大きいものを任意に選ぶ。cの両端をp,qとする。
g2からさらにcを除いたグラフg3を作り、g3でpからqが到達不可能ならば、これが3辺の最後の1本と確信できる。

以上の手順の中で、偶然が起きなかった場合、最終的にcの候補で条件を満たすものがなく終わる。

aの候補は辺の本数だけあるが、bの候補はiからjへの経路の長さだけで、最悪でも頂点数-1で抑えられる。
実際にはもっと短いだろうから、$O(M^3)$のような莫大な計算量にはならないはず。

上の手順の部品から作っていく。

グラフ `Graph` から頂点iとjを直接結ぶ辺を両向きで除く`delEdge`

```haskell
delEdge :: Graph -> Int -> Int -> Graph
delEdge g i j = accum del g [(i,j),(j,i)]
  where
    del xs y = filter ((y /=) . fst) xs
```

グラフにおいて頂点iからjへの最多経路をBFSで探し、辺の背番号の（逆順の）リストで返す`findPath`

```haskell
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

findPath :: Graph -> Int -> Int -> [Int]
findPath g i j = runST $
  do
    dist <- newArray (bounds g) noPath :: ST s (STArray s Int [Int])
    writeArray dist i []
    loop dist [i] []
    readArray dist j
  where
    noPath = [-1]
    loop _ist [] [] = return ()
    loop dist [] news = loop dist news []
    loop dist (i:is) news = do
      disti <- readArray dist i
      news1 <- foldM (\ns (j,a) -> do
        distj <- readArray dist j
        if distj /= noPath then return ns else do
          writeArray dist j $ a : disti
          return $ j : ns
        ) news (g ! i)
      loop dist is news1
```

グラフにおいて頂点iから到達できる頂点の個数と、頂点jから到達できる頂点の個数をDFSで求める`reachCnt`
ただし、iとjが連結である場合には`Nothing`を返す。

```haskell
reachCnt g i j = runST $
  do
    fld <- newArray (bounds g) False :: ST s (STArray s Int Bool)
    writeArray fld i True
    x <- iter fld i 0
    fj <- readArray fld j
    if fj then return Nothing else do
      writeArray fld j True
      y <- iter fld j 0
      return $ Just (x,y)
  where
    iter fld p cnt0 = succ <$>
      foldM (\cnt (q,_) -> do
        f <- readArray fld q
        if f then return cnt else do
          writeArray fld q True
          iter fld q cnt
        ) cnt0 (g ! p)
```

以上の部品を組み合わせると、上記のアルゴリズムが実現できる。

```haskell
part1 ls =
      [ (x, y, x * y)
-- 辺aを1つ（順に）選び、aを除いたグラフg1を作り
      | a <- [1 .. m], let (i,j) = edges ! a, let g1 = delEdge graph0 i j
-- g1でのiからjへの経路を作り
      , let p1 = findPath g1 i j
-- 経路上の全ての辺b、ただしaより大きいものを順に一つ選び、bを除いたグラフg2を作り
      , b <- p1, a < b, let (k,l) = edges ! b, let g2 = delEdge g1 k l
-- g2でのiからj, kからlへの経路を作り
      , let p2 = findPath g2 i j, let p3 = findPath g2 k l
-- その共通する辺c、ただしaより大きいものを順に一つ選び、cも除いたグラフg3を作り
      , c <- intersect p2 p3, a < c, let (p,q) = edges ! c, let g3 = delEdge g2 p q
-- g3でiからjが到達不能なときの、
-- iから到達できる頂点の個数とjから到達できる頂点の個数が答えをなす
      , Just (x,y) <- [reachCnt g3 i j]
      ]
  where
    ...
```

サンプルは一瞬、本番データは正解そのものは程ほどの時間待ちで出力されるものの、プログラムが停止しない。
総当たりで他に解がない、唯一解であることの確認がとれないのは気持ち悪い。
どうしようかと思ったが、コンパイル実行したら一瞬で終わった。
STモナドがインタプリタだと遅いらしいのが原因か？
