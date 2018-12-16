{-# Language Strict #-}

{-
unfoldを使うのもうまくいかなそうだ。
現在までのリストを参照しないと作れないから。

[3,7] スコアボードの最後の二つ、とは限らないからやはりリストで。
現在のリストの長さ
0,1 elf1 elf2 の位置

と、無限リストによるスコアボードそのもの、かしら。
ランダムアクセスしないときついかもしれないのだけど。
-}

{-
initial = ([3,7],(2,(0,1)))

scoreboard = concatMap fst record
record = initial : step initial
-}

scoreboard = 3 : 7 : loop 2 0 1
loop len e1 e2
  | s > 9 = 1 : s - 10 : rest
  | True  =     s      : rest
  where
    s1 = scoreboard !! e1
    s2 = scoreboard !! e2
    s = s1 + s2
    len1 = len + if s > 9 then 2 else 1
    f1 = (e1 + 1 + s1) `mod` len1
    f2 = (e2 + 1 + s2) `mod` len1
    rest = loop len1 f1 f2

compute1 n = concatMap show $ take 10 $ drop n scoreboard

test1 = compute1 9
test2 = compute1 5
test3 = compute1 18
test4 = compute1 2018
ans1 = compute1 793031

-- ans1がでない。さすがに線形アクセスで80万はきつい。
