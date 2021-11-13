q1 n = last loop
  where
    loop = q1iter n ([1..n] ++ loop)

q1iter 1 (x:_) = [x]
q1iter n (x:y:xs) = x : q1iter (n-1) xs

input = 3014387

{-
*Main> q1 input
1834471
-}
