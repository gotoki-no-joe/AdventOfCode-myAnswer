import Data.Array
import Data.Array.IO
import Control.Monad

type XYZ = (Int,Int,Int)

parse :: String -> XYZ
parse s = read $ '(' : s ++ ")"

getLH :: (XYZ -> Int) -> [XYZ] -> (Int,Int)
getLH f cs = (pred $ minimum vs, succ $ maximum vs)
  where
    vs = map f cs

neighbors :: XYZ -> [XYZ]
neighbors (x,y,z) = [(pred x,y,z),(x,pred y,z),(x,y,pred z)
                    ,(succ x,y,z),(x,succ y,z),(x,y,succ z)]

compute1 :: [XYZ] -> Int
compute1 xyzs = length
    [() | xyz <- xyzs, xyz1 <- neighbors xyz, not $ arr ! xyz1]
  where
    (xL,xH) = getLH (\(x,_,_) -> x) xyzs
    (yL,yH) = getLH (\(_,y,_) -> y) xyzs
    (zL,zH) = getLH (\(_,_,z) -> z) xyzs
    arr = accumArray (||) False ((xL,yL,zL),(xH,yH,zH)) [(xyz,True) | xyz <- xyzs]

phase1 fn = print . compute1 . map parse . lines =<< readFile fn

test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

phase2 fn = print =<< compute2 . map parse . lines =<< readFile fn

compute2 :: [XYZ] -> IO Int
compute2 cs =
  do
    arr <- newArray bnds 0 :: IO (IOArray XYZ Int)
    forM_ cs $ \xyz -> writeArray arr xyz 1
    loop 0 arr [(xL,yL,zL)]
  where
    (xL,xH) = getLH (\(x,_,_) -> x) cs
    (yL,yH) = getLH (\(_,y,_) -> y) cs
    (zL,zH) = getLH (\(_,_,z) -> z) cs
    bnds = ((xL,yL,zL),(xH,yH,zH))

    loop :: Int -> IOArray XYZ Int -> [XYZ] -> IO Int
    loop cnt _ [] = return cnt
    loop cnt arr (xyz:xyzs) =
      do
        v <- readArray arr xyz
        case v of
          1 -> loop (succ cnt) arr xyzs
          2 -> loop cnt arr xyzs
          0 ->
            do
              writeArray arr xyz 2
              loop cnt arr $ filter (inRange bnds) (neighbors xyz) ++ xyzs
          _ -> error ""

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"
