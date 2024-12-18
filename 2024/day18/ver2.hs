import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Heap as H

runner i f = do
  xys <- map parse . lines <$> readFile i
  print $ f xys

parse :: String -> (Int, Int)
parse l = (read as, read bs)
  where
    (as, _:bs) = break (',' ==) l

test1 = runner "samp1.txt" $ part1 6 . take 12
main1 = runner "input.txt" $ part1 70 . take 1024

part1 :: Int -> [(Int,Int)] -> Int
part1 ub xys = dist
  where
    bnds = ((0,0),(ub,ub))
    fld = accumArray (&&) True bnds [(xy, False) | xy <- xys]
    l = index bnds $ fst bnds
    u = index bnds $ snd bnds

    dist = runST $ do
      arr <- dijkstra (l,u) edge (index bnds (0,0))
      readArray arr $ index bnds (ub,ub)

    i2xy = listArray (l,u) $ range bnds
    edge i
      | fld ! (x,y) = [(index bnds p, 1) | p <- [(pred x,y),(succ x,y),(x,pred y),(x,succ y)], inRange bnds p]
      | otherwise   = []
      where
        (x,y) = i2xy ! i

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

binarySearch :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binarySearch prop unsat sat = loop unsat sat
  where
    loop a b
      | ende   = (a, b)
      | prop m = loop a m
      | True   = loop m b
      where
        ende = a == m || b == m
        m = div (a + b) 2

test2 = runner "samp1.txt" $ part2 6
main2 = runner "input.txt" $ part2 70

part2 ub xys = xys !! pred out
  where
    prop k = part1 ub (take k xys) < maxBound
    (out, safe) = binarySearch prop (length xys) 0
