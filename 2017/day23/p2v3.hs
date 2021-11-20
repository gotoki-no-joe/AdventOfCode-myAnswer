import Data.Numbers.Primes

{-
105700から+17きざみで122700以下の数bで、2以上(b未満)の2数の積で表せるもの、
つまり素数でないものの数を数えている。
-}

ans = length $ filter (not . isPrime) [105700,105717..122700]

{-
*Main> ans
915
-}
