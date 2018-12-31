{-# Language Strict #-}

import qualified Data.IntMap as M

elf k = fromAscList $ take 50 $ [(i, k*11)| i <- [k, k+k..]]

compute2 = iter 1 M.empty where
  iter k m = m1 M.! k : iter (succ k) m2
    m1 = M.unionWith (+) m (elf k)
    m2 = M.delete m1 k

theinput = 36000000

ans2 = length $ takeWhile (theinput >) compute2

{- やっぱり重い。無理。 -}
