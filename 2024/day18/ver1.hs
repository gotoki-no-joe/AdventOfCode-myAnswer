{-
サンプルは [0,6]^2 範囲
本番は[0,70]^2 範囲
(0,0) から (ub,ub) に移動する

1024バイトとは、input.txt のサイズでなくて、
入力の1行が「1バイトが降ってくる」なので、それだな。
-}

import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Heap as H

import Data.List

import Debug.Trace

runner i f = do
  xys <- map parse . lines <$> readFile i
  print $ f xys

parse :: String -> (Int, Int)
parse l = (read as, read bs)
  where
    (as, _:bs) = break (',' ==) l

test1 = runner "samp1.txt" $ part1 6 . take 12
main1 = runner "input.txt" $ part1 70 . take 1024

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

{-
パート2
3450程度なら、線形探索でもいけると思うよ？
-}

test2 = runner "samp1.txt" $ part2 6
main2 = runner "input.txt" $ part2 70

part2 ub xys0 = head
  [ (k, last xys)
  | (k,xys) <- tail $ zip [0 ..] $ inits xys0
  , mod k 100 /= 0 || traceShow k True
  , compDist xys > div maxBound 2
  ]
  where
    bnds = ((0,0),(ub,ub))
    l = index bnds $ fst bnds
    u = index bnds $ snd bnds
    i2xy = listArray (l,u) $ range bnds

    compDist xys = dist
      where
        fld = accumArray (&&) True bnds [(xy, False) | xy <- xys]

        dist = runST $ do
          arr <- dijkstra (l,u) edge (index bnds (0,0))
          readArray arr $ index bnds (ub,ub)

        edge i
          | fld ! (x,y) = [(index bnds p, 1) | p <- [(pred x,y),(succ x,y),(x,pred y),(x,succ y)], inRange bnds p]
          | otherwise   = []
          where
            (x,y) = i2xy ! i

{-
想像以上に重かった。だから二分探索で見つけろってことだよね。了解です。
-}

-- @gotoki_no_joe
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

test2a = runner "samp1.txt" $ part2a 6
main2a = runner "input.txt" $ part2a 70

part2a ub xys0 = xys0 !! pred out
  where
    bnds = ((0,0),(ub,ub))
    l = index bnds $ fst bnds
    u = index bnds $ snd bnds
    i2xy = listArray (l,u) $ range bnds

    prop k = compDist (take k xys0) < div maxBound 2
    (out, safe) = binarySearch prop (length xys0) 0

    compDist xys = dist
      where
        fld = accumArray (&&) True bnds [(xy, False) | xy <- xys]

        dist = runST $ do
          arr <- dijkstra (l,u) edge (index bnds (0,0))
          readArray arr $ index bnds (ub,ub)

        edge i
          | fld ! (x,y) = [(index bnds p, 1) | p <- [(pred x,y),(succ x,y),(x,pred y),(x,succ y)], inRange bnds p]
          | otherwise   = []
          where
            (x,y) = i2xy ! i
