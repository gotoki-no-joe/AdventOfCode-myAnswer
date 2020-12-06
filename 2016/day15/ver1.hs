{-
時刻tにボタンを押すと、
時刻t+iにディスクiに到達する。
そのとき、ディスクiの位置が0なら成功、さもなくば失敗。
全て成功するタイミングを見つけろ、と。

各ディスクは、周期とオフセットをパラメータとして持っている。
位置k周期aオフセットbなディスクが、その位置で0をもたらす出発時刻tは、
k+t+b = 0 mod a
-}

{-
example
Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.
-}

test1 = head [t | t <- [0..], (1+t+4) `mod` 5 == 0, (2+t+1) `mod` 2 == 0]

{-
my input
Disc #1 has 13 positions; at time=0, it is at position 11.
Disc #2 has 5 positions; at time=0, it is at position 0.
Disc #3 has 17 positions; at time=0, it is at position 11.
Disc #4 has 3 positions; at time=0, it is at position 0.
Disc #5 has 7 positions; at time=0, it is at position 2.
Disc #6 has 19 positions; at time=0, it is at position 17.
-}

example = [(1,5,4),(2,2,1)]

puzzle = [(1,13,11),(2,5,0),(3,17,11),(4,3,0),(5,7,2),(6,19,17)]

phase1 ds = head $ filter (check1 ds) [0..]

check1 ds t = and [mod (k+t+b) a == 0 | (k,a,b) <- ds]

phase2 = head $ filter (check1 (puzzle++[(7,11,0)])) [0..]

{-
5
*Main> phase1 example
5
*Main> phase1 puzzle
122318
*Main> phase2
3208583

力任せに解いてしまった。
-}
