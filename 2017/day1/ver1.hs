import System.CPUTime
import Data.Char

main = do
  t1 <- getCPUTime
  li <- readFile "input.txt"
--  print li
  let li1 = filter isDigit li
  let ans = compute li1
  print ans
  t2 <- getCPUTime
  print $ t2 - t1
  putStrLn "part two"
  let ans2 = compute2 li1
  print ans2
  t3 <- getCPUTime
  print $ t3 - t2

compute :: String -> Int
compute li =
  sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip li (tail li ++ [head li])

t1 = compute "1122"
t2 = compute "1111"
t3 = compute "1234"
t4 = compute "91212129"

{-
getCPUTimeはpicoSecond単位 = 10^-12sec
要求は1ms以下 = 10-^3sec

91212129で試して
15,625,000,000
と出たり(アウト)
0
と出たり。
-}

compute2 :: String -> Int
compute2 li = ans where
  (as,bs) = splitAt (length li `div` 2) li
  ans = sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip li (bs++as)

{-
*Main> main
1119
0
part two
1420
0
-}
