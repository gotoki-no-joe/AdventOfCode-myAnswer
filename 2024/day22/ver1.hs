import Data.Bits
import Data.Array.Unboxed
import Data.List.Split
import Data.List

prune :: Int -> Int
prune x = 16777215 .&. x
-- prune x = mod x 16777216

mix :: Int -> Int -> Int
mix = xor

secretStep x = x3
  where
    x1 = prune $ mix x  $ shiftL x  6 -- x * 64 -- shiftL x 6
    x2 = prune $ mix x1 $ shiftR x1 5 -- div x2 32 -- shiftR x 5
    x3 = prune $ mix x2 $ shiftL x2 11 -- x3 * 2048 -- shiftL x 11

runner i f = do
  ns <- map read . lines <$> readFile i
  print $ f ns

part1 :: [Int] -> Int
part1 ns = sum [iterate secretStep n !! 2000 | n <- ns]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

-- 2363行あると結構かかるもんだな。

{-
ghci> test1
37327623
ghci> main1
19854248602
-}

{-
ふむ、これなら解けそう。

一人のバイヤーのシードについて、2000個の数の列を作る。part1で実行済み。
下1桁で価格の列にする。
差の列を考える。前から、4つの差の列をキーにして、最初に現れる位置のスコアを記録する。
先頭のだけなので、二つめ以降は無視する。
差は-9から9まで起きうるが、実の値が9を越えないので ((-9,-9,-9,-9),(9,9,9,9)) は無駄が結構ある。
19^4 = 130321 要素 無駄があっても、いちいち列でやって Map を使うよりも高速でしょう。

全員のこの配列について、要素ごとに足し合わせた最大値を答えればいい。
-}

type ARR = UArray (Int,Int,Int,Int) Int

bnds :: ((Int, Int, Int, Int), (Int, Int, Int, Int))
bnds = ((-9,-9,-9,-9),(9,9,9,9))

compute1 :: Int -> ARR
compute1 seed = arr
  where
    prices = map (flip mod 10) $ take 2001 $ iterate secretStep seed
    deltas = zipWith (-) (tail prices) prices
    arr = accumArray set (-1) bnds
        [ ((d1,d2,d3,d4),s)
        | ([d1,d2,d3,d4],s) <- zip (divvy 4 1 deltas) (drop 4 prices)]

set (-1) x = x
set x _ = x

mergeArrays :: [ARR] -> ARR
mergeArrays arrs = listArray bnds $ map (sum . map (max 0)) $ transpose $ map elems arrs

-- ここまでを試す
test2 = mergeArrays [compute1 x | x <- [1,2,3,2024]] ! (-2,1,-1,3)

main2 = runner "input.txt" part2

part2 ns = maximum $ elems arr
  where
    arr = mergeArrays [compute1 x | x <- ns]

{-
ghci> test2
23
ghci> main2
2223
-}
