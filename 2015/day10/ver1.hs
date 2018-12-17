{-# Language Strict #-}

import Data.List

input :: [Int]
input = [3,1,1,3,3,2,2,1,1,3]

step ns = concatMap (\ms -> [length ms, head ms]) $ group ns

test1 = take 6 $ iterate step [1]

-- 10以上のカウントが出ると困る。桁ごとの分解処理が必要になる。
check1 = filter (10 <=) $ concat $ take 41 $ iterate step input
-- ならなかった。

ans1 = length $ (iterate step input) !! 40

till50 = take 10 $ drop 41 $ iterate step input

check2 = filter (10 <=) $ concat till50

ans2 = length $ last till50

{-
*Main> test1
[[1],[1,1],[2,1],[1,2,1,1],[1,1,1,2,2,1],[3,1,2,2,1,1]]
*Main> check1
[]
*Main> ans1
329356
*Main> check2
[]
*Main> ans2
4666278
-}
