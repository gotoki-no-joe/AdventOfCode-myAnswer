{-
全体をグラフで表現してダイクストラ法で。

マスについて、座標と方向で背番号を振る。
有向グラフにすれば、構築は楽。
壁のマスからは、どこへも出辺を張らない。
そうでないマスについて、
- 向きに一歩進んで同じ向きなノードに距離1
- 同じマスで左右にターンしたノードに距離1000
スタートは'S'のE向き
ゴールとして、'G'の4マスから距離0で辺を引いて、そこを特別扱いする。
-}

import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import qualified Data.Heap as H

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test0 = runner "samp1.txt" part1
test1 = runner "samp2.txt" part1
main1 = runner "input.txt" part1

dN, dE, dW, dS :: Int
dN = 0
dE = 1
dW = 2
dS = 3
walk (i,j) d =
  case d of
    0 -> (pred i, j)
    1 -> (i, succ j)
    2 -> (i, pred j)
    3 -> (succ i, j)

turn 0 = [1,2]
turn 1 = [0,3]
turn 2 = [0,3]
turn 3 = [1,2]

part1 :: [String] -> (Int, Int)
part1 ls = (part1ans, part2ans)
  where
    h = length ls
    w = length $ head ls
    fld = listArray ((1,1),(h,w)) $ concat ls :: UArray (Int,Int) Char
-- 頂点番号の範囲
    bnds = (((1,1),0),((h,w),3))
-- ゴールを一つにするために、本来は壁の中の ((1,1),0) を仮想のゴールと…しない！
    goal  = head [ij | (ij, 'E') <- assocs fld]
-- 辺を張る
    nodesbnds = (index bnds ((1,1),0), index bnds ((h,w),3))
    xedni = listArray nodesbnds $ range bnds :: Array Int ((Int, Int), Int) -- 番号からノードを復元する
    edge i
      | fld ! ij == '#' = []
      | otherwise = (index bnds (walk ij d, d), 1) : [(index bnds (ij,d1), 1000) | d1 <- turn d]
      where
        (ij, d) = xedni ! i
-- 開始点
    start  = head [ij | (ij,'S') <- assocs fld]
    dist :: UArray Int Int
    dist = runSTUArray $ dijkstra nodesbnds edge (index bnds (start,dE))
-- パート1答え
    part1ans = minimum [dist ! index bnds (goal, d) | d <- [0 .. 3]]

-- パート2
-- 本当にゴールになっている、距離part1ansの頂点を突き止める
    realgoals = [i | d <- [0 .. 3], let i = index bnds (goal, d), dist ! i == part1ans]
-- 遅延配列DPで、最適経路上のノードを洗い出す
    bestpathnodes = listArray nodesbnds $ map bestpathnodesfun $ range nodesbnds :: Array Int Bool
    bestpathnodesfun i
      | elem i realgoals = True
      | otherwise = or [bestpathnodes ! j | (j,w) <- edge i, dist ! i + w == dist ! j]
    part2ans = length
      [ ()
      | ij <- range ((1,1),(h,w)), or [bestpathnodes ! index bnds (ij, d) | d <- [0 .. 3]]]

{-
パート2

しばらく意味がわからなかったけれど、
・コストが同じ、同着一位な経路がいくつかある
・そのいずれかに属するマスを全て数えよ
といってる。

パート1を拡張して、最短経路のいずれかに含まれるノードを全て取り出す。
それは、そうであるノードからの距離と、始点からの距離の差が、辺の長さにぴったり一致するところ。

距離が短くなる方に下っていくばかりなので、訪問済みかどうかは気にせず、
edge と dist だけを使って…違う、ゴールから到達可能でないといけないので、
ちゃんとBFSする必要がある。

いや、ハイキングのときのことを思い出すと、
より距離の遠い隣接点の誰かに、ゴールから到達可能となっていれば、
自分も到達可能というlazy array dpができて一発だ！
-}

-- @gotoki_no_joe
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
