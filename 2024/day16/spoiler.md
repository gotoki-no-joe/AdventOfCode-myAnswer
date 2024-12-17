# 入力

マップを二次元配列にして渡す。

```haskell
import Data.Array.Unboxed

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test0 = runner "samp1.txt" part1
test1 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

part1 :: UArray (Int,Int) Char -> Int
part1 arr = ...
```

# パート1

ダイクストラ法で距離を求めることで解ける。
1つのマス（タイル）ごとに、トナカイがどっちを向いているか、という情報も乗せて4つの頂点を割りあてる。

同じマスの、左右に回転した状態を表す頂点へ、距離1000の辺を張る。
自分の向きの隣接マスに一歩前進した（向きは変わらないまま）の状態を表す頂点へ、距離1の辺を張る。

マップで `#` な壁の中のマスからは辺を張らない。
床から壁へは辺を張ってしまうことになるが、出て行くことができない有向グラフなので結果には影響しない。

出発地点は `S` のあるマスの東向きとする。
ゴール地点 `E` のマスの、4方向全ての向きそれぞれに対応する頂点の距離を調べ、その最小値が答えである。

まず、手持ちの自作ダイクストラ法のAPIと実装を示す。

```haskell
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Heap as H

dijkstra :: (Int, Int)           -- 頂点番号の範囲
         -> (Int -> [(Int,Int)]) -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                  -- 開始点
         -> ST s (STUArray s Int Int)
dijkstra bnds edges start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    loop <- fixST $ \loop -> return $ \queue ->
      if H.null queue then return dist else do
        let Just (H.Entry cost u, queue1) = H.uncons queue
        du <- readArray dist u
        if du < cost then loop queue1 else do
          queue2 <- foldM (\q (v, we) -> do
            let duv = du + we
            dv <- readArray dist v
            if dv <= duv then return q else do
              writeArray dist v duv
              return $ H.insert (H.Entry duv v) q
            ) queue1 (edges u)
          loop queue2
    loop $ H.singleton (H.Entry 0 start)
```

## 第１引数

頂点は整数で与える。下限と上限は自由に指定できる。
今回は2次元座標×方向の3次元の要素を持つ要素に頂点を割り当てる必要があるので、
`Data.Ix` を使って一次元に投影する。

```haskell
    (ll,hw) = bounds fld   -- フィールドの座標範囲 ((1,1),(h,w)) を取り出す
    bnds = ((ll,0),(hw,3)) -- 4方向を表す整数 0 ～ 3 をもう一次元加えたものが頂点の要素
```

これで、`index bnds ((i,j),d)` とすれば、頂点を表現する整数が得られる。
その整数の下限と上限が `dijkstra` の第1引数になる。

```haskell
    nodesbnds = (index bnds (ll,0), index bnds (hw,3))
```

向きの番号にも名前を割り当てておく。関連する関数も作っておく。

```haskell
-- 4方向
[dN, dE, dW, dS] = [0 .. 3] :: [Int]

-- 一歩先の座標
walk :: (Int,Int) -> Int -> (Int,Int)
walk (i,j) d =
  case d of
    0 -> (pred i, j)
    1 -> (i, succ j)
    2 -> (i, pred j)
    3 -> (succ i, j)

-- 左右旋回した向きリスト
turn :: Int -> [Int]
turn 0 = [1,2]
turn 1 = [0,3]
turn 2 = [0,3]
turn 3 = [1,2]
```

## 第2引数

グラフの辺は、配列で与える代わりに、それを与える関数で指定するように一般化している。
ノード番号が引数に与えられるので、接続先のノード番号と辺の重みの対のリストを返す。

ここで、一次元に投影されたノード番号しか得られないので、元の座標と方向の要素に戻す逆写像の配列を作っておく。

```haskell
    xedni = listArray nodesbnds $ range bnds :: Array Int ((Int, Int), Int) -- 番号からノードを復元する
```

これを補助関数として使うことで、辺を与える関数が定義できる。

```haskell
    edge i
      | fld ! ij == '#' = [] -- 壁なら辺は出ない
      | otherwise = (index bnds (walk ij d, d), 1) : [(index bnds (ij,d1), 1000) | d1 <- turn d] -- 前進と旋回
      where
        (ij, d) = xedni ! i
```

## 第3引数

`S` のある座標を取り出す。

```haskell
    start  = head [ij | (ij,'S') <- assocs fld]
```

この位置の東向きのノード番号は `index bnds (start, dE)` である。

## 戻り値

結果は `ST s (STUArray Int Int)` で返されるので、`runSTUArray` で `UArray Int Int` に固定しておく。
この配列の、ゴールの位置の4方向の距離の最小値がパート1の答えである。

```haskell
    dist :: UArray Int Int
    dist = runSTUArray $ dijkstra nodesbnds edge (index bnds (start, dE))
-- パート1答え
    goal  = head [ij | (ij, 'E') <- assocs fld]
    part1ans = minimum [dist ! index bnds (goal, d) | d <- [0 .. 3]]
```

# パート2

問題文が少しわかりにくいが、最小スコアを達成する最良の経路は一つだけでなく複数存在するので、
それらを全て見つけて、一度でも最良経路が通過するマスの個数を答えよ、といっている。

今手許にはスタートからの距離 `dist` と、もちろんそれぞれの辺の重み `edge` もあるので、
ゴールから始めて、「現在のノードの距離ー隣接ノードの距離＝その辺の重み」という関係を満たすノードは、
そこを通る最良経路のあるノードとわかる。
命令型言語ならこれをbfsなどで発見することになるだろう。

しかしHaskellなら、10日めにやったように、遅延評価配列を使って集めるDPが簡単に実現できる。

- そのノードが最短経路上にあるとき True を持つことになる配列を用意する。
- 最短距離になっているゴールのノードは、最短経路の一部である。
- それ以外のノードは、自分の周囲のノードのうち、自分よりスタートから遠いノードだけについて、
スタートからの距離の差が辺の長さと一致し、かつそのノードが True を持っているようなノードが一つでもあるなら、
このノードもまた最短経路の一部である。

こうしてノードに関して最短経路を選別した後、方向について無視することで、マスの個数を数えることができる。
パート1で求まる、スタートからの距離の表が必要なので、`part1`にそのまま`part2`の答えを求めるこーとを追加しよう。

```haskell
--part1 :: UArray (Int,Int) Char -> Int
--part1 fld = part1ans
part12 :: UArray (Int,Int) Char -> (Int,Int)
part12 fld = (part1ans, part2ans)
  where
    ...
-- パート2
-- 本当にゴールになっている、距離part1ansの頂点を突き止める
    realgoals = [i | d <- [0 .. 3], let i = index bnds (goal, d), dist ! i == part1ans]
-- 遅延配列DPで、最適経路上のノードを洗い出す
    bestpathnodes = listArray nodesbnds $ map bestpathnodesfun $ range nodesbnds :: Array Int Bool
    bestpathnodesfun i
      | elem i realgoals = True
      | otherwise = or [bestpathnodes ! j | (j,w) <- edge i, dist ! i + w == dist ! j]
-- パート2答え
    part2ans = length
      [ ()
      | ij <- range (ll,hw), or [bestpathnodes ! index bnds (ij, d) | d <- [0 .. 3]]]
```

## 追記

10日めのspoilerで関数 `dp` を定義したので、これを使って書き直してみよう。

```haskell
    bestpathnodes = dp dist gather neighbors
    gather i jds
      | elem i realgoals = True
      | otherwise        = any snd jds
    neighbors i = [j | let di = dist ! i, (j, w) <- edge i, di + w == dist ! j]
```

できたが、`UArray` と `Array` の混在で型エラーになるのを直せなくて、全て `Array` にして誤魔化した。
