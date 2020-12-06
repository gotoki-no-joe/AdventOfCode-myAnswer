{-
modをいちいち計算するのではなく、それぞれのdiscがいい感じになる時刻をstreamで出力し、
二つのstreamがどちらも出力するものだけ出力していく感じでやったらもっと速いはず。
-}

{-
位置k周期aオフセットbなディスクが、その位置で0をもたらす出発時刻tは k+t+b = 0 mod a
そのような時刻は k+t+b = na, t = na - k - b
-}

disc k a b = dropWhile (0 >) $ iterate (a +) (-k-b)

merge xxs@(x:xs) yys@(y:ys) = case compare x y of
    EQ -> x : merge xs ys
    LT -> merge xs yys
    GT -> merge xxs ys

example = [(1,5,4),(2,2,1)]

puzzle = [(1,13,11),(2,5,0),(3,17,11),(4,3,0),(5,7,2),(6,19,17)]

phase1 ds = head $ foldl1 merge [disc k a b | (k,a,b) <- ds]

phase2 = phase1 (puzzle++[(7,11,0)])

{-
*Main> phase1 example
5
*Main> phase1 puzzle
122318
*Main> phase2
3208583
やはり少し時間はかかるが、前のよりはずっと速い。
-}
