{-
row 2978, column 3083.
-}

-- theseq = iterate step 20151125
step x = x * 252533 `mod` 33554393

{-
row, column から、何番目なのかを取り出したい。

ナナメの列を1,2,3行と数える。
k行めの長さはk その右上端は 1+2+...+k = k(k+1)/2 番目の要素。
r,c のマスはk+c-1行めのc番目の要素。

つまり全体ではそれは (k+c-1)*(k+c)/2 + c 番目の要素になる。
-}

theRow = 2978
theCol = 3083

ans1 = theseq !! idx where
  theseq = iterate step 20151125
  kc = theRow + theCol
  idx = (kc - 1) * kc `div`2 + theCol - 1
