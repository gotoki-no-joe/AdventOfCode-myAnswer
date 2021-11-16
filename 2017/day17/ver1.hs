{-
復元版

現在の長さと、現在位置を先頭にしたリストとを持つことで、実際に手順を行う。
現在の長さは、次に挿入するべき値でもある。
-}

part1 n = part1loop n 1 [0]

part1loop _ 2018 xs = xs !! 1
part1loop n k xs = part1loop n (succ k) (k : bs ++ as)
  where
    k1 = succ $ mod n k
    (as,bs) = splitAt k1 xs

test1 = part1 3

ans1 = part1 359

{-
*Main> test1
638
*Main> ans1
1506
-}
