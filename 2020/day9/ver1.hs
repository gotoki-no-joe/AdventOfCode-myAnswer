{-
過去n個の数の組み合わせn(n-1)/2の和に、次の数xが含まれているかを何度も判定する。
x-xiがx1～xnに含まれているかを調べるしかない？
「含まれている」だけ、線形探索でなくIntSetを使う感じで。
-}

import qualified Data.IntSet as S
import Data.List

sample = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

check1 :: Int -> [Int] -> [Int] -- 幅
check1 w xs = loop (S.fromList $ take w xs) xs (drop w xs)
  where
    loop _ _ [] = []
    loop s xxs@(x:xs) (y:ys)
      | isOK = next
      | True = y : next
      where
        isOK = not $ null [ () | x <- take w xxs, S.member (y-x) s ]
        next = loop (S.insert y $ S.delete x s) xs ys

test1 = check1 5 sample

ans1 = do
  co <- readFile "input.txt"
  print $ check1 25 $ map read $ lines co

{-
よくある、連続する範囲の和を効率的にとる話。
x1～xkの和を1<=k<=nについてとって、skとして、j<kについて
sk-sj = x(j+1) + ... + xk
とすればいい。
-}

findspan :: Int -> [Int] -> [[Int]]
findspan x xs = [drop i (take j xs) | ((i,si):iss1) <- tails iss, (j,sj) <- iss1, sj - si == x]
  where
    iss = zip [1..] $ scanl1 (+) xs

test2 = findspan 127 sample

ans2 = do
  co <- readFile "input.txt"
  let xs = map read $ lines co
  let x = head (check1 25 xs)
  print x
  let yss = findspan x xs
--  print yss
  let ys = head yss
  print ys
  let ew = minimum ys + maximum ys
  print ew

{-
*Main> test1
[127]
*Main> ans1
[393911906]
*Main> test2
[[15,25,47,40],[127]]
*Main> ans2
393911906
[17315331,16773507,16828690,20664316,18358238,18793333,19486189,24772011,20285093,30609544,20354435,21183505,21265569,42568378,27563219,27207191,29883357]
59341885
-}
