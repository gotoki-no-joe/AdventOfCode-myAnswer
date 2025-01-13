{-
-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Bits
import Data.List

dijkstra :: Int                  -- 頂点数N (1～N)
         -> (Int -> [(Int,Int)]) -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                  -- 開始点
         -> ST s (STArray s Int Int)
dijkstra n edges start =
  do
    dist <- newArray (1,n) maxBound
    writeArray dist start 0
    rh <- newRH
    pushRH 0 start rh
    loop dist rh
  where
    loop dist rh = do
      mcu <- popRH rh
      case mcu of
        Nothing -> return dist
        Just (cost, u) -> do
          du <- readArray dist u
          when (du == cost) $
            forM_ (edges u) (\(v, we) -> do
              let d1 = du + we
              dv <- readArray dist v
              when (d1 < dv) $ do
                writeArray dist v d1
                pushRH d1 v rh
              )
          loop dist rh

-- Radix Heap

radixDist :: Int -> Int -> Int
radixDist x y = sub (xor x y)
  where
    sub 0 = 0
    sub z = succ $ sum $ snd $ mapAccumL step z [32,16,8,4,2,1]
    step z w =
      case shiftR z w of
        0  -> (z , 0)
        z1 -> (z1, w)

data RadixHeap s a = RadixHeap (STRef s Int) (STArray s Int [(Int,a)])

newRH :: ST s (RadixHeap s a)
newRH = do
  r <- newSTRef 0
  arr <- newListArray (0,63) $ repeat []
  return $ RadixHeap r arr

pushRH :: Int -> a -> RadixHeap s a -> ST s ()
pushRH p x (RadixHeap lp arr) =
  do
    l <- readSTRef lp
    if p < l then error (unwords ["assert fail", show p, "<", show l]) else
      modifyArray arr (radixDist l p) ((p, x) :)

popRH :: RadixHeap s a -> ST s (Maybe (Int, a))
popRH (RadixHeap lp arr) =
  do
    ipxs <- dropWhile (null . snd) <$> getAssocs arr
    case ipxs of
      [] -> return Nothing
      (i, pxs) : _ -> do
        writeArray arr i []
        let lx@(l, _) = minimumBy (\(i,_) (j,_) -> compare i j) pxs
        forM_ pxs (\py@(p, _) -> modifyArray arr (radixDist l p) (py :))
        writeArray arr 0 [] -- でいいんだよな？
        writeSTRef lp l
        return $ Just lx

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

test1 = runST $ do
  rh <- newRH
  mapM_ (\p -> pushRH p p rh) [10, 9 .. 1]
  replicateM 10 (popRH rh)

-- pediaの例
test2 = runSTArray $ dijkstra 6 ef 1
  where
    ef 1 = [(2,7),(3,9),(6,14)]
    ef 2 = [(1,7),(3,10),(4,15)]
    ef 3 = [(1,9),(2,10),(4,11),(6,2)]
    ef 4 = [(2,15),(3,11),(5,6)]
    ef 5 = [(4,6),(6,9)]
    ef 6 = [(1,14),(3,2),(5,9)]
