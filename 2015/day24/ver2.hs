-- 2022-11-25
import System.IO.Unsafe
import Data.List
import Data.Bits

{-# NOINLINE xs #-}
xs :: [Int]
xs = unsafePerformIO (map read . lines <$> readFile "input.txt")

{-
> divMod (sum xs) 3
(516,0)

合計がこれになるような全ての選択を考える。
> length xs
28

ビット総当たりでそれぞれの取り方を見分けて、さらに、共存する分割の仕方かを判断できる。
(荷物の個数, QE)をキー、ビットパターンを値とするリストを作る。重量が1/3のものだけ。
キーの昇順に総当たりで収まりが良いものを探せば、題意を満たせる。
-}

cands :: [(Int,Int,Int)]
cands = sort [(popCount b, p, b) | b <- [1..all1-3], let (s,p) = bits2wqe b, s == 516]

bits2wqe b = (sum xs1, product xs1)
  where
    xs1 = [x | (i,x) <- zip [0..27] xs, testBit b i]

all1 = 2^28 - 1

part1 =
  [ (b1,p1)
  | (_,p1,b1):bps1 <- tails cands
  , (_,_ ,b2):bps2 <- tails bps1, b1 .&. b2 == 0, let b12 = b1 .|. b2
  , (_,_ ,b3)      <- bps2, b12 .&. b3 == 0, b12 .|. b3 == all1
  ]

-- length cands が終わらない。naiveすぎた。