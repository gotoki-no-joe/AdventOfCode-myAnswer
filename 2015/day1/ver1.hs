main = do
  l <- readFile "input.txt"
  let ans1 = compute l
  print ans1

p2f '(' = succ
p2f ')' = pred

compute :: String -> Int
compute l = foldl (flip ($)) 0 $ map p2f l

{-
*Main> main
74
-}
