import Data.List
import Data.List.Split

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
  | m `mod` 23 == 0 = let (as,b:bs) = splitAt (length xs - 7) xs
                      in  m+b : simulate (bs++as) ms
  | singleton xs = 0 : simulate (m:xs) ms
  | otherwise = let (as,bs) = splitAt 2 xs
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
