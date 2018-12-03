-- https://adventofcode.com/2018/day/3

import qualified Data.IntMap as M

{-
#1 @ 808,550: 12x22
#2 @ 486,680: 13x15

こんな形式のファイルを読んで、重複して使われている領域の面積を返す。
idはどうでもいい。

開始位置からサイズに渡る区画をタプルで表現して、
重複した登録を別にカウントする、といった感じか。

斜めに数え上げることで整数に写像した方が、
IntMapとか使えて便利なんだけども。
どうやるんだっけ？

136af
259e
48d
7c
b

n,x,yは1始まりで考える。
斜めのn列めの長さはn
斜めのn列めの最後はsum 1 n = n(n+1)/2
つまりn列めの先頭はn(n-1)/2+1
(x,y)はx-1+y-1+1=x+y-1列めに属し、そのy番目になる。つまり、
n=x+y-1である先頭にy-1を足した数になる、つまり
n(n-1)/2+y where n=x+y-1

必要ないけれど、ついでにNからX,Yを求める方法も考えておくか？
面倒だから置いておく。

-}

xy2k x y = n * (n-1) `div` 2 + y where n = x+y-1

-- test = [xy2k x y | x <- [1..5], y <- [1..5]]

{-
#1 @ 808,550: 12x22
#2 @ 486,680: 13x15

形式の行を読み取るには、パーサを書くかちまちまやるか。
-}

compline :: String -> (Int,Int,Int,Int)
compline li = (read cx, read cy, read cw, read ch) where
  l1 = drop 2 $ dropWhile ('@' /=) li
  (cx,_:l2) = break (',' ==) l1
  (cy,_:_:l3) = break (':' ==) l2
  (cw,_:ch) = break ('x' ==) l3

-- t1 = compline "#1 @ 808,550: 12x22"
-- t2 = compline "#2 @ 486,680: 13x15"

genlocs :: (Int,Int,Int,Int) -> [Int]
genlocs (x,y,w,h) = [ xy2k a b | a <- [x..x+w-1], b <- [y..y+h-1] ]

main = do
  file <- readFile "input.txt"
  let result = compute file
  print result

compute :: String -> Int
compute file = length $ filter (1 <) $ M.elems themap where
  locs = concatMap genlocs $ map compline $ lines file
  themap = foldl (\m loc -> M.insertWith (+) loc 1 m) M.empty locs

{-
Main>
105231
-}
