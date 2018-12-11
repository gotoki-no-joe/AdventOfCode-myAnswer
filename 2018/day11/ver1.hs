{-# Language Strict #-}

import Data.Array

type Grid = Array (Int,Int) Int

genGrid gridid = array ((1,1),(300,300)) list where
  list =
    [((x,y), (((x+10) * y + gridid)*(x+10)) `div` 100 `mod` 10 - 5)
    | x <- [1..300], y <- [1..300] ]

test1 = [genGrid 8 ! (3,5), genGrid 57 ! (122,79), genGrid 39 ! (217,196), genGrid 71 ! (101,153)]

findmax cells = maximum
  [ (sum [cells ! (a,b)| a <- [x..x+2], b <- [y..y+2]],(x,y)) | x <- [1..300-2], y <- [1..300-2] ]

test2 = [findmax (genGrid 18), findmax (genGrid 42)]

ans1 = findmax (genGrid 9005)

{-
*Main> test1
[4,-5,0,4]*Main> test2
[(29,(33,45)),(30,(21,61))]
*Main> ans1
(31,(20,32))
-}

{-
カリカリにチューニングはしなくても、足し算の回数をほどほどにケチる。

n-1 1
 x  :
n-1 n-1
1.. n

Sn+1 (x,y) = Sn(x,y) + Σ_{k=0..n} a_(x+k,y+n) + a_(x+n,y+k) - a(x+n,y+n)
-}

findnmax cells = maximum $ concatMap elems (elems ss) where
  ss = listArray (1,300) [ sizeSum k | k <- [1..300] ]
  sizeSum 1 = cells
  sizeSum k = array ((1,1),(301-k,301-k)) [ ((x,y), ss ! (k-1) ! (x,y) + sum [ cells ! (x+i,y+k) + cells ! (x+k,y+i) | i <- [1..k-1]] + cells ! (x+k,y+k)) | x <- [1..301-k], y <- [1..301-k] ]

test3 = [findnmax (genGrid 18), findnmax (genGrid 42)]

ans2 = findnmax (genGrid 9005)

{-
メモリがあふれてWinGHCiが落ちました。とほほ。
-}
