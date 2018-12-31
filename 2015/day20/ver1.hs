{-
10倍は目くらましなのでスルー。
-}

{-
k番の妖精がそれぞれの家にいくつのプレゼントを配るかを無限リストで作る。
ただし、1番の家から考えると無限の妖精のリストを足す必要があるので、
k番はk番めから考える。
-}

-- elf k = cycle (succ k:replicate 0 k)

{-
これをk番めにはkを追加で足すことで、合計を得る。
-}

-- presents =
--   elf k = cycle (k:replicate 0 (k-1))

{-
というやり方はわけわからんので、
k番目はk番目の系列と、一つずらしてk+1番目の系列を足すという方針で。
-}

presents = seq 1 where
  seq k = k : zipWith (+) (cycle (replicate (k-1) 0 ++ [k])) (seq (k+1))

theinput = 36000000

ans1 = length $ takeWhile (theinput `div` 10 >) presents

{-
というやり方は重くてぜんぜんダメでした。
-}
