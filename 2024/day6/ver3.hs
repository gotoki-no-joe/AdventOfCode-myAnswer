import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

rot :: (Int,Int) -> (Int,Int)
rot (dx,dy) = (dy, - dx)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c,b+d)

part1 :: UArray (Int, Int) Char -> Int
part1 arr = runST $ do
      stain <- newArray bnds False -- 足跡
      writeArray stain pos0 True   -- 初期位置を踏む
      walking stain pos0 (-1,0)
      length . filter id <$> getElems stain
  where
    bnds = bounds arr
    -- 守衛の最初の位置
    pos0 = head [p | (p, '^') <- assocs arr]
    walking :: STUArray s (Int, Int) Bool
            -> (Int, Int) -> (Int, Int) -> ST s ()
    walking stain pos dir
      | not (inRange bnds pos1) = return () -- 踏み出すと落ちるなら終わり
      | arr ! pos1 /= '#' = writeArray stain pos1 True >> walking stain pos1 dir -- 進めるなら進む
      | otherwise = walking stain pos (rot dir) -- 進めないなら右を向く
      where
        pos1 = add pos dir

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: UArray (Int, Int) Char -> Int
part2 arr = length
    [ ()
    | (obs, True) <- assocs strain
    , let arr2 = arr // [(obs, '#')]
    , causeLoop arr2 ]
  where
    bnds@(l,u) = bounds arr
    -- 守衛の最初の位置
    pos0 = head [p | (p, '^') <- assocs arr]
    -- 障害物なしで踏む場所
    strain = runSTUArray $ do
      stain <- newArray bnds False -- 足跡
      walking stain pos0 (-1,0)
      writeArray stain pos0 False   -- 初期位置を除外
      return stain
    walking :: STUArray s (Int, Int) Bool
            -> (Int, Int) -> (Int, Int) -> ST s ()
    walking stain pos dir
      | not (inRange bnds pos1) = return () -- 踏み出すと落ちるなら終わり
      | arr ! pos1 /= '#' = writeArray stain pos1 True >> walking stain pos1 dir -- 進めるなら進む
      | otherwise = walking stain pos (rot dir) -- 進めないなら右を向く
      where
        pos1 = add pos dir
    causeLoop arr2 = runST $ do
      strain <- newArray ((l,0),(u,3)) False
      walking2 arr2 strain pos0 (-1,0)
    walking2 :: UArray (Int,Int) Char
             -> STUArray s ((Int, Int), Int) Bool
             -> (Int, Int) -> (Int, Int) -> ST s Bool
    walking2 arr2 stain pos dir
      | not (inRange bnds pos1) = return False        -- 落ちた
      | arr2 ! pos1 /= '#' = do                       -- 進める
          b <- readArray stain (pos1, d2i dir)
          if b then return True else do               -- ループ検出
            writeArray stain (pos1, d2i dir) True
            walking2 arr2 stain pos1 dir              -- 進む
      | otherwise = walking2 arr2 stain pos (rot dir) -- 進めないなら右を向く
      where
        pos1 = add pos dir

d2i :: (Int,Int) -> Int
d2i (-1,0) = 0
d2i ( 0,1) = 1
d2i ( 1,0) = 2
d2i (0,-1) = 3

main = test1 >> main1 >> test2 >> main2
