-- 2025/1/14 spoilerのためにやり直し

import Data.List
import Data.Array
import Data.Tuple

runner i f = readFile i >>= print . f . lines

-- most common character
mcc xs = snd $ maximum $ map swap $ assocs $ accumArray (+) 0 ('a','z') [(x,1) | x <- xs]

part1 = map mcc . transpose

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

{-
ghci> test1
"easter"
ghci> main1
"wkbvmikb"
-}

-- least common letter
lcl xs = snd $ minimum $ map swap $ filter ((0 <) . snd) $ assocs $ accumArray (+) 0 ('a','z') [(x,1) | x <- xs]

part2 = map lcl . transpose

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
"advent"
ghci> main2
"evakwaga"
-}
