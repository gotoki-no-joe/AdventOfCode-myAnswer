-- {-# Language BangPatterns #-}
-- {-# Language Strict #-}

import Data.List
import Data.List.Split
import Control.DeepSeq
import System.CPUTime

{-
単純にシミュレーションかけていけばいいのだけど、回るのが面倒だな。
現在の位置をリストの先頭に必ずするか、
位置を別の変数で追跡するか。

何番目のビー玉が何点になったかを出力する。
集計は外でする。

あーこれ、mutable double link list を使うのが正解だね…

-}

simulate :: [Int] -- 盤面の状況
         -> [Int] -- 次に置く石
         -> [Int] -- 各手番における得点
simulate xs [] = []
simulate xs (m:ms)
  | m `mod` 23 == 0 = let (as,b:bs) = force $ splitAt (length xs - 7) xs
                      in  m+b : simulate (bs++as) ms
  | singleton xs = 0 : simulate (m:xs) ms
  | otherwise = let (as,bs) = force $ splitAt 2 xs
                in 0 : simulate (m:bs ++ as) ms

singleton [_] = True
singleton _ = False

runsim p m = sort $ flip zip [1..] $ map sum $ transpose $ chunksOf p $ simulate [0] [1..m]

test0 = runsim 9 25
test1 = runsim 10 1618
test2 = runsim 13 7999
test3 = runsim 17 1104
test4 = runsim 21 6111
test5 = runsim 30 5807

ans1 = runsim 486 70833

{-
*Main> ans1
[(208920,370),(216607,85),...,(371561,209),(373597,324)]
-}

ans2 = runsim 486 7083300

main = print $ last ans2

timeit x = do
  t0 <- getCPUTime
  let ans = force x
  print ans
  t1 <- getCPUTime
  print (t1-t0)

{-
素の状態              Language Strict force $ splitAt
*Main> timeit ans1
59671875000000        46875000000     ?
*Main> timeit test5
312500000000          15625000000     390625000000
*Main> timeit test4
421875000000           0              484375000000
*Main> timeit test3
15625000000            0              31250000000
*Main> timeit test2
703125000000           0              859375000000
*Main> timeit test1
15625000000            0              31250000000
*Main> timeit test0
0                      0              0

おお、すげぇ速くなってる。まだ遅いけど。
forceではうまくいかない、というかそれだけではないということか？
-}
