import Data.List
import Data.Array

sample1 = [16,10,15,5,1,11,7,19,6,12,4]

comp1 xs = (ones * threes, length ds - ones - threes, sort ds)
  where
    xs1 = sort (0:3+maximum xs:xs)
    ds = zipWith (-) (tail xs1) xs1
    ones = length $ filter (1 ==) ds
    threes = length $ filter (3 ==) ds

sample2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]

ans1 = do
  co <- readFile "input.txt"
  print $ comp1 $ map read $ lines co

{-
後半は、プロコンでおなじみな、合流する数を数えるDPだね。
-}

comp2 xs = ja ! bin
  where
    bin = maximum xs + 3
-- なんで止まる？
--    ja = accumArray (flip const) 0 (-2,bin) ((0,1):[ (j,ja ! (j-3) + ja ! (j-2) + ja ! (j-1)) | j <- bin:xs])
-- これなら動く
    ja = listArray (-2,bin) (0:0:1:[if elem j xs || j == bin then ja ! (j-3) + ja ! (j-2) + ja ! (j-1) else 0 | j <- [1..bin] ])

comp21 xs = loop 0 0 1 1 (sort (bin:xs))
  where
    bin = maximum xs + 3
    loop j3 j2 j1 j (v:vs)
      | j == v = loop j2 j1 (j3+j2+j1) (succ j) vs
      | True   = loop j2 j1 0          (succ j) (v:vs)
    loop _ _ j1 _ [] = j1

ans2 = do
  co <- readFile "input.txt"
  print $ comp21 $ map read $ lines co

{-
*Main> comp1 sample1
(35,0,[1,1,1,1,1,1,1,3,3,3,3,3])
*Main> comp1 sample2
(220,0,[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3])
*Main> ans1
(2484,0,[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
*Main> comp21 sample1
8
*Main> comp21 sample2
19208
*Main> ans2
15790581481472
*Main> comp2 sample1
8
*Main> comp2 sample2
19208
*Main> readFile "input.txt" >>= print . comp2 . map read . lines
15790581481472
-}
