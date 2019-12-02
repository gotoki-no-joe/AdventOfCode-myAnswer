import Data.Array

main = do
    cont <- readFile "input.txt"
    let ls = lines cont
    let ws = map read ls
    let ans = compute ws
    print ans

compute :: [Int] -> Int
compute ws = sum $ map f ws
  where
    f x = x `div` 3 - 2

{-
*Main> main
3126794
-}

main2 = do
    cont <- readFile "input.txt"
    let ls = lines cont
    let ws = map read ls
    let ans = compute2 ws
    print ans

compute2 :: [Int] -> Int
compute2 ws = sum $ map (gs !!) ws

fs = map f [0..]
  where
    f x = max 0 (x `div` 3 - 2)

gs = map g [0..]
  where
    g n | n <= 0 = 0
        | True   = let a = fs !! n in a + gs !! a

{-
*Main> main2
4687331
-}

-- 上限を見て配列でDPするように改善

main3 = do
  cont <- readFile "input.txt"
  let ws = map read $ lines cont
  let ans = compute3 ws
  print ans

compute3 :: [Int] -> Int
compute3 ws = sum $ map (fa !) ws
  where
    ub = maximum ws
    f x = if x <= 0 then 0 else fa ! x
    fa = listArray (0,ub) [ w + f w | x <- [0..ub], let w = max 0 (x `div` 3 - 2) ]
