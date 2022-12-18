{-
どれだけ広がっているかわからないから、3-tupleのsetで表現して、
±1の要素があるならその面は数えない、
で愚直に数えよう。
-}

import qualified Data.Set as S
import Data.Array.IO
import Control.Monad

type XYZ = (Int,Int,Int)

parse :: String -> XYZ
parse s = read $ '(' : s ++ ")"

phase1 fn = do
  cs <- map parse . lines <$> readFile fn
  print $ compute1 cs

compute1 :: [XYZ] -> Int
compute1 cs = length
  [ ()
  | (x,y,z) <- cs
  , (x1,y1,z1) <- [(pred x,y,z),(succ x,y,z),(x,pred y,z),(x,succ y,z),(x,y,pred z),(x,y,succ z)]
  , S.notMember (x1,y1,z1) s
  ]
  where
    s = S.fromList cs

test0 = compute1 [(1,1,1),(2,1,1)]
test1 = phase1 "test.txt"
main1 = phase1 "input.txt"

{-
パート2どうすんだこれ。
xyzの限界読んで、その一回り広い範囲までボクセルで考えて、
明らかに外側な原点あたりから連結成分を埋めて、
その内側な面を数える、ぐらいかな。簡単なのは。面倒だけど。
というか、ペイントしていくときに、cubeに当たった回数を数えておけば、
それがそのまま答えか。
-}

test2 = phase2 "test.txt"
main2 = phase2 "input.txt"

phase2 fn = do
  cs <- map parse . lines <$> readFile fn
  print =<< compute2 cs

-- 0 は未処理
-- 1 はCube
-- 2 は外の水

compute2 :: [XYZ] -> IO Int
compute2 cs =
  do
    arr <- newArray ((xL,yL,zL),(xH,yH,zH)) 0 :: IO (IOArray XYZ Int)
    forM_ cs $ \xyz -> writeArray arr xyz 1
    loop 0 arr [(xL,yL,zL)]
  where
-- 境界範囲を探す
    (xL,xH) = getLH (\(x,_,_) -> x) cs
    (yL,yH) = getLH (\(_,y,_) -> y) cs
    (zL,zH) = getLH (\(_,_,z) -> z) cs
-- 1広げているので、(xL,yL,zL) にcubeは存在しない。

getLH :: (XYZ -> Int) -> [XYZ] -> (Int,Int)
getLH f cs = (pred $ minimum vs, succ $ maximum vs)
  where
    vs = map f cs

loop :: Int -> IOArray XYZ Int -> [XYZ] -> IO Int
loop cnt _ [] = return cnt
loop cnt arr (xyz@(x,y,z):xyzs) =
  do
    v <- readArray arr xyz
    case v of
      1 -> loop (succ cnt) arr xyzs
      2 -> loop cnt arr xyzs
      0 ->
        do
          writeArray arr xyz 2
          bnds <- getBounds arr
          let xyzs1 = [(pred x,y,z),(succ x,y,z),(x,pred y,z),(x,succ y,z),(x,y,pred z),(x,y,succ z)]
          loop cnt arr $ filter (inRange bnds) xyzs1 ++ xyzs
      _ -> error "never"
