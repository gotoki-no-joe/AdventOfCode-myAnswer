-- 2022-11-24

import Data.Array.Unboxed

import qualified Data.IntMap as IM

theInput :: Int
theInput = 36000000
theInput10 = div theInput 10

part1 = head $ filter ((theInput10 <=) . snd) $ assocs arr
  where
    arr :: UArray Int Int
    arr = accumArray (+) 1 (1,theInput10)
          [(j, i) | i <- [2 .. theInput10], j <- [i, i+i .. theInput10]]

-- おお、これならさっくり動いた。少し待つけど。

part1a = loop 1 100
  where
    loop lb ub
      | null ans = loop (succ ub) (min theInput10 (ub * 2))
      | otherwise = head ans
      where
        arr :: UArray Int Int
        arr = accumArray (+) 1 (lb,ub)
          [(j, i) | i <- [2..ub], let j0 = i * divrup lb i, j <- [j0, j0+i .. ub]]
        ans = filter ((theInput10 <=) . snd) $ assocs arr

-- @gotoki_no_joe
divrup x y = negate $ div (negate x) y

{-
*Main> part1a
(831600,3690240)
(13.15 secs, 10,312,119,152 bytes)
*Main> part1
(831600,3690240)
(19.87 secs, 19,128,033,728 bytes)

*Main> part1
(831600,3690240)
(19.84 secs, 19,128,034,016 bytes)
*Main> part1a
(831600,3690240)
(13.36 secs, 10,312,118,864 bytes)

分割型の方が速くてメモリも節約できている。
-}

-- part2は、naiveに計算する他の手を思いつかない。

part2 = loop 1 IM.empty
  where
    loop k im
      | n >= theInput = (k, n)
      | otherwise     = loop (succ k) im1
      where
        ((k1, n), im1) =
          IM.deleteFindMin $
          IM.unionWith (+) im $
          IM.fromDistinctAscList $
          [(i, 11 * k) | i <- map (k *) [1 .. 50]]

{-
> part2
(884520,36191925)
動いた。
-}

part2a = loop 1 IM.empty
  where
    loop k im
      | n >= theInput = (k, n)
      | otherwise     = loop (succ k) im1
      where
        ((k1, n), im1) =
          IM.deleteFindMin $
          foldl (\im (i,k) -> IM.insertWith (+) i k im) im $
          [(i, 11 * k) | i <- map (k *) [1 .. 50]]

{-
*Main> part2
(884520,36191925)
( 89.38 secs, 53,054,476,184 bytes)
*Main> part2a
(884520,36191925)
(149.70 secs, 63,931,157,936 bytes)

変にfoldlでやる方が、時間も空間も無駄になっている。
-}

part2b = loop 1 IM.empty
  where
    loop k im
      | n >= theInput = (k, n)
      | otherwise     = loop (succ k) im1
      where
        im1 =
          IM.unionWith (+) im $
          IM.fromDistinctAscList $
          [(i, 11 * k) | i <- map (k *) [1 .. 50]]
        (_k,n) = IM.findMin im1

-- 小さい値を消さない方がメモリ操作回数を減らせるかとも思ったが、
-- 古い値が消えずに溜まっていくと、毎回の操作も重くなるので善し悪しだ。
-- というかCtrl-Cも受け付けないレベルで固まってしまった。明らかに失敗ってことだ。
