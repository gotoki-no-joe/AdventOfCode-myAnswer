import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Bits
import Data.List

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
        writeArray arr 0 [] -- でいいんだよな？
        writeSTRef lp l
        return $ Just lx

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

test1 = runST $ do
  rh <- newRH
  mapM_ (\p -> pushRH p p rh) [10, 9 .. 1]
  replicateM 10 (popRH rh)
