import qualified Data.IntMap as M

{-
数に対して、それが最後に呼ばれたターンを覚えるIntMapを使う。

ループに関して、直前の時刻、直前の数x、直前のマップの3つが必要。
-}

comp1 xs = xs1 ++ loop t0 x m0
  where
    xs1 = init xs
    m0 = M.fromList $ zip xs1 [1..]
    t0 = length xs
    x = last xs

loop t x m
  | M.notMember x m = x : loop t1 0 m1
  | True            = x : loop t1 a m1
  where
    t1 = succ t
    m1 = M.insert x t m
    a = t - (m M.! x)

test1 = take 10 $ comp1 [0,3,6]
test2 = (!! 2019) $ comp1 [0,3,6]
test3 = map ((!! 2019) . comp1) [[1,3,2],[2,1,3],[1,2,3],[2,3,1],[3,2,1],[3,1,2]]

ans1 = (!! 2019) $ comp1 [0,13,1,16,6,17]

{-
*Main> test1
[0,3,6,0,3,3,1,0,4,0]
*Main> test2
436
*Main> test3
[1,10,27,78,438,1836]
*Main> ans1
234
*Main> (!! 29999999) $ comp1 [0,3,6]
175594
*Main> (!! 29999999) $ comp1 [0,13,1,16,6,17]
8984

力任せにやると当然時間がかかるが、待っていれば答えが出た。
効率的に答えを求める方法があるとは思えない…
-}
