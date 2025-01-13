import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Bits
import Data.List

radixDist :: Int -> Int -> Int
radixDist x y = sub (xor x y)
  where
    sub 0 = 0
    sub z = succ $ sum $ snd $ mapAccumL step z [32,16,8,4,2,1]
    step z w =
      case shiftR z w of
        0  -> (z , 0)
        z1 -> (z1, w)

data RadixHeap s = RadixHeap (STRef s Int) (STArray s Int [Int])

newRH = do
  r <- newSTRef 0
  arr <- newListArray (0,63) $ repeat []
  return $ RadixHeap r arr

pushRH :: Int -> RadixHeap s -> ST s ()
pushRH v (RadixHeap p arr) =
  do
    l <- readSTRef p
    if v < l then error (unwords ["assert fail", show v, "<", show l]) else
      modifyArray arr (radixDist l v) (v :)

popRH :: RadixHeap s -> ST s Int
popRH (RadixHeap p arr) =
  do
    (i, xs) <- head . dropWhile (null . snd) <$> getAssocs arr
    writeArray arr i []
    let l = minimum xs
    forM_ xs (\x -> modifyArray arr (radixDist l x) (x :))
    writeArray arr 0 [] -- でいいんだよな？
    writeSTRef p l
    return l

modifyArray :: (MArray a b m, Ix i) => a i b -> i -> (b -> b) -> m ()
modifyArray arr i f = readArray arr i >>= writeArray arr i . f

test1 = runST $ do
  rh <- newRH
  mapM_ (flip pushRH rh) [10, 9 .. 1]
  replicateM 10 (popRH rh)
