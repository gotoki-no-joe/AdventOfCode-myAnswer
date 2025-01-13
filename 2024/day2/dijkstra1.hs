import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Bits
import Data.List

dijkstra :: (Int,Int)            -- 頂点番号下限と上限
         -> (Int -> [(Int,Int)]) -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                  -- 開始点
         -> ST s (STArray s Int Int)
dijkstra bnds edges start =
  do
    dist <- newArray bnds maxBound
    writeArray dist start 0
    rh <- newRH
    pushRH 0 start rh
    join $ fixST $ \loop -> return $ do
      mcu <- popRH rh
      case mcu of
        Nothing -> return ()
        Just (cost, u) -> do
          du <- readArray dist u
          when (du == cost) $
            forM_ (edges u) $ \(v, we) -> do
              let duv = du + we
              dv <- readArray dist v
              when (duv < dv) $ do
                writeArray dist v duv
                pushRH duv v rh
          loop
    return dist

-- ようやく使えるようになった。
example = runST $ do
  r <- newSTRef 0
  action <- fixST $ \loop -> return (\() -> do
    v <- readSTRef r
    when (v < 10) $ do
      writeSTRef r (succ v)
      loop ()
    )
  action ()
  readSTRef r

example2 = runST $ do
  r <- newSTRef 0
  join $ fixST $ \loop -> return $ do
    v <- readSTRef r
    when (v < 10) $ do
      writeSTRef r (succ v)
      loop
  readSTRef r

doLoop :: (ST s a -> ST s a) -> ST s a
doLoop f = join $ fixST $ \loop -> return $ f loop

example3 = runST $ do
  r <- newSTRef 0
  doLoop $ \loop -> do
    v <- readSTRef r
    when (v < 10) $ do
      writeSTRef r (succ v)
      loop
  readSTRef r

-- Radix Heap

radixDist :: Int -> Int -> Int
radixDist x y =
  case xor x y of
    0  -> 0
    xy -> succ $ sum $ snd $ mapAccumL step xy [32,16,8,4,2,1]
  where
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
        writeArray arr 0 []
        writeSTRef lp l
        return $ Just lx

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

test1 = runST $ do
  rh <- newRH
  mapM_ (\p -> pushRH p p rh) [10, 9 .. 1]
  replicateM 10 (popRH rh)

test2 = runSTArray $ dijkstra (1,6) ef 1
  where
    ef 1 = [(2,7),(3,9),(6,14)]
    ef 2 = [(1,7),(3,10),(4,15)]
    ef 3 = [(1,9),(2,10),(4,11),(6,2)]
    ef 4 = [(2,15),(3,11),(5,6)]
    ef 5 = [(4,6),(6,9)]
    ef 6 = [(1,14),(3,2),(5,9)]
