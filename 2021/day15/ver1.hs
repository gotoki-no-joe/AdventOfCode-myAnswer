{-
右と下にしか進まない、とは言ってないんだよなぁ...
とりあえずそうだと思い込んで解くか。
-}

import Data.Char
import Data.List

compute1 :: String -> Int
compute1 xs = last acc - d0
  where
    ls = lines xs
    dss = map (map digitToInt) ls
    d0 = head $ head dss
    acc = foldl' step (0 : repeat 10000) dss

step :: [Int] -> [Int] -> [Int]
step scores ds = tail scores1
  where
    scores1 = maxBound : zipWith3 f scores scores1 ds
    f s t d = d + min s t

test1 = readFile "sample.txt" >>= print . compute1

run1 = readFile "input.txt" >>= print . compute1

-- part2 5倍にするだけ？

compute2 :: String -> Int
compute2 xs = last acc - d0
  where
    ls = lines xs
    dss = mag5 $ map (map digitToInt) ls
    d0 = head $ head dss
    acc = foldl' step (0 : repeat 10000) dss

incr 9 = 1
incr n = succ n

mag5 :: [[Int]] -> [[Int]]
mag5 dss = dss2
  where
    dss1 = map (concat . take 5 . iterate (map incr)) dss
    dss2 = concat $ take 5 $ iterate (map (map incr)) dss1

test2 = readFile "sample.txt" >>= print . compute2

run2 = readFile "input.txt" >>= print . compute2

{-
*Main> test1
40
*Main> run1
769
*Main> test2
315
*Main> run2
2970

違うって言われた。この期に及んで回り道ありとか言い出すわけ？
-}
