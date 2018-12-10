{- Language Strict -}

import Data.List
import qualified Data.Set as S
import qualified Data.Array as A

main = do
  fi <- readFile "input.txt"
  let bs = map read $ words fi
  let n = length (bs :: [Int])
  let ba = A.listArray (0,n-1) bs
  let ans = compute n ba
  print (fst ans)
  let ans2 = compute n (snd ans)
  print (fst ans2)

sample = [0,2,7,0]

compute n ba = (length loop, last loop) where
  loop = unfoldr (step n) (ba, S.empty)

step n (ba, m)
  | S.member ba m = Nothing
  | otherwise = Just (ba,(ba1, S.insert ba m))
  where
    bs = A.elems ba
    x = maximum bs
    i = length $ takeWhile (x >) bs
    ba1 = A.accum (+) ba ((i,-x):[((i+j) `mod` n, 1) | j <- [1..x] ])

test1 = compute 4 (A.listArray (0,3) sample)

test2 = compute 4 (snd test1)

{-
*Main> test1
(5,array (0,3) [(0,1),(1,3),(2,4),(3,1)])
*Main> test2
(4,array (0,3) [(0,0),(1,2),(2,3),(3,4)])
*Main> main
4074
2793

arrayをSetの要素にするとかなかなかだな。
-}
