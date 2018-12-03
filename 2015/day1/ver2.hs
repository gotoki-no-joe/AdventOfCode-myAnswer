main = do
  l <- readFile "input.txt"
  let ans1 = compute l
  print ans1
  let ans2 = compute2 l
  print ans2

p2f '(' = succ
p2f ')' = pred

compute :: String -> Int
compute l = foldl (flip ($)) 0 $ map p2f l

{-
*Main> main
74
-}

compute2 l = length $ takeWhile (-1 <) $ scanl (flip ($)) 0 $ map p2f l
