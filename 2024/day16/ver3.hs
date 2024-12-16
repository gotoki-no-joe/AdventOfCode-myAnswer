import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import qualified Data.Heap as H

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      fld = listArray ((1,1),(h,w)) $ concat ls
  print $ f fld

test0 = runner "samp1.txt" part12
test1 = runner "samp2.txt" part12
main1 = runner "input.txt" part12

--part1 :: UArray (Int,Int) Char -> Int
--part1 fld = part1ans
part12 :: UArray (Int,Int) Char -> (Int,Int)
part12 fld = (part1ans, part2ans)
  where
-- 頂点番号の範囲
    (ll,hw) = bounds fld
    bnds = ((ll,0),(hw,3))
-- ゴールの位置
    goal  = head [ij | (ij, 'E') <- assocs fld]
-- 辺を張る
    nodesbnds = (index bnds (ll,0), index bnds (hw,3))
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
      | ij <- range (ll,hw), or [bestpathnodes ! index bnds (ij, d) | d <- [0 .. 3]]]

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
