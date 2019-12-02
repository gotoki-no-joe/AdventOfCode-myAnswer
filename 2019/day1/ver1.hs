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
