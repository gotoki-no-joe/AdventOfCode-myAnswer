import Data.Bits

initial = (634, 301) :: (Int,Int)
example = (65, 8921) :: (Int,Int)

genAfactor = 16807
genBfactor = 48271

step :: Int -> Int -> Int
step fac x = x * fac `mod` 2147483647

generator :: Int -> Int -> [Int]
generator fac start = tail $ iterate (step fac) start

judge (a,b) = a .&. b15 == b .&. b15

b15 = bit 16 - 1

pairs (startA,startB) = zip (generator genAfactor startA) (generator genBfactor startB)

compute1 = length . filter judge . take 40000000 . pairs

ans1 = compute1 initial

{-
*Main> compute1 example
588
*Main> ans1
573
-}

pairs2 (startA,startB) =
  zip (filter ((0 ==).(3 .&.)) $ generator genAfactor startA)
      (filter ((0 ==).(7 .&.)) $ generator genBfactor startB)

compute2 = length . filter judge . take 5000000 . pairs2

ans2 = compute2 initial
