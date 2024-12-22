import Data.Bits
import Data.Array.Unboxed
import Data.List

runner i f = do
  ns <- map read . lines <$> readFile i
  print $ f ns

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> Int
part1 ns = sum [iterate secretStep n !! 2000 | n <- ns]

secretStep :: Int -> Int
secretStep x = w
  where
    y = prune $ mix x $ shiftL x 6
    z = prune $ mix y $ shiftR y 5
    w = prune $ mix z $ shiftL z 11
    mix = xor
    prune x = 16777215 .&. x

type ARR = UArray (Int,Int,Int,Int) Int

bnds :: ((Int, Int, Int, Int), (Int, Int, Int, Int))
bnds = ((-9,-9,-9,-9),(9,9,9,9))

countBuyer :: Int -> ARR
countBuyer seed = arr
  where
    ps = map (flip mod 10) $ take 2001 $ iterate secretStep seed
    ds = zipWith (-) (tail ps) ps
    d4s = zip4 ds (drop 1 ds) (drop 2 ds) (drop 3 ds)
    arr = accumArray set (-1) bnds $ zip d4s (drop 4 ps)

    set (-1) x = x
    set x _ = x

mergeArrays :: [ARR] -> ARR
mergeArrays arrs = listArray bnds $ map (sum . map (max 0)) $ transpose $ map elems arrs

part2 ns = (ans, seq4s)
  where
    arr = mergeArrays $ map countBuyer ns
    ans = maximum $ elems arr
    seq4s = [i | (i,e) <- assocs arr, e == ans]

main2 = runner "input.txt" part2
