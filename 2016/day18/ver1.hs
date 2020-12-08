import System.CPUTime

sample1 = "..^^."
sample2 = ".^^.^.^^^^"
input = "^^^^......^...^..^....^^^.^^^.^.^^^^^^..^...^^...^^^.^^....^..^^^.^.^^...^.^...^^.^^^.^^^^.^^.^..^.^"

trap b c = b /= c

nextRaw bs = zipWith trap b1 b2
  where
    b1 = False:bs
    b2 = drop 1 bs ++ [False]

phase1 str lines = length $ filter not $ concat $ take lines $ iterate nextRaw $ map ('^' ==) str
phase11 str lines = sum $ map (length . filter not) $ take lines $ iterate nextRaw $ map ('^' ==) str

observe str lines = putStrLn $ unlines $ map (map f) $ take lines $ iterate nextRaw $ map ('^' ==) str
  where
    f b = if b then '^' else '.'

{-
*Main> observe sample1 3 
..^^.
.^^^^
^^..^

*Main> observe sample2 10
.^^.^.^^^^
^^^...^..^
^.^^.^.^^.
..^^...^^^
.^^^^.^^.^
^^..^.^^..
^^^^..^^^.
^..^^^^.^^
.^^^..^.^^
^^.^^^..^^

*Main> phase1 sample2 10
38
*Main> phase1 input 40
1978
*Main> phase1 input 400000
20003246

後半、力任せに求めてしまったけれど、何かうまい方法があるのだろうか。
パターンがループしていることを突き止めるのはなかなか大変だけど。

一応、壁も含めて奇数番目と偶数番目は独立に、相手側の次の状態を生成するから、
それぞれで考えることかできて、場合の数が倍にできるぶん、ループの可能性も高くなる。
最後にまとめる段階が面倒くさいが。
-}

run e = do
  t1 <- getCPUTime
  print e
  t2 <- getCPUTime
  print (t2-t1)

{-
*Main> run (phase1 input 400000)
20003246
11406250000000
*Main> run (phase11 input 400000)
20003246
21812500000000
遅延評価の実行時間は謎だのぉ。
-}
