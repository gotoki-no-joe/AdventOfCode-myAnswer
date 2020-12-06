{-
もっと解析的にこの問題は解ける。

位置k周期aオフセットbなディスクが初めて位置0になる時刻は k+t+b = 0 mod a となるtの最小値
t = na - k - b = -k-b mod a
これをt0とし、位相と呼ぶことにする。

周期a1、位相t01なディスクと、周期a2、位相t02なディスクが同時に位置0になる最初の時刻tが
t = n1a1+t01 = n2a2 + t02
であるとき、他のものは a = lcm a1 a2 を用いて、
周期a、位相tなディスクひとつと等しい。

tはver2と同じ方法で求めて、aは作れて、これでひとつのディスクの情報に集約する。
-}

conv32 (k,a,b) = (a, (-k-b) `mod` a)

example = [(1,5,4),(2,2,1)]

puzzle = [(1,13,11),(2,5,0),(3,17,11),(4,3,0),(5,7,2),(6,19,17)]

stream (a,t) = iterate (a +) t

merge xxs@(x:xs) yys@(y:ys) = case compare x y of
    EQ -> x : merge xs ys
    LT -> merge xs yys
    GT -> merge xxs ys

phase d1 d2 = head $ merge (stream d1) (stream d2)

unify d1 d2 = (lcm (fst d1) (fst d2), phase d1 d2)

compute ds = snd $ foldl1 unify $ map conv32 ds

{-
*Main> compute example
5
*Main> compute puzzle
122318
*Main> compute (puzzle++[(7,11,0)])
3208583

これがきっと究極回答でしょう。
-}
