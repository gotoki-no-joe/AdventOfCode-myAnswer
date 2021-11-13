{-
全体が偶数個2nのとき、同数の半分ずつ(n,n)に割るとする。
1...n / n+1....2n
前半分の先頭が、後ろ半分の先頭をなくす (n,n-1)
o(1)2...n / x(n+1)n+2...2n
終わった先頭が後ろの末尾に並ぶ (n-1,n)
2..n / n+2...2n,1

全体が奇数個2n+1のとき、後ろを多くする(n,n+1)
1..n / n+1...2n,2n+1
前半分の先頭が、後ろ半分の先頭をなくす (n,n)
o(1)2...n / x(n+1)n+2...2n,2n+1
終わった先頭が後ろの末尾に並ぶ (n-1,n+1)
2..n / n+2...2n,2n+1,1
数をバランスさせるために一人前にでる (n,n)
2..n,n+2 / n+3...2n,2n+1,1

これは、むしろ切れ目のところで要素の減少が起きて、
つなぎ目はただ繋がっているだけなので、
そのようにするとよい。
-}

input = 3014387

q2 n = last loop
  where
    m = div n 2
    loop = q2iter n ([m+1..n] ++ [1..m] ++ loop)

q2iter 1 (x:_) = [x]
q2iter n (x:xs)
  | even n = q2iter (pred n) xs
q2iter n (x:y:xs)
  | odd n = y : q2iter (pred n) xs

{-
*Main> q2 5
2
*Main> q2 input
1420064
-}
