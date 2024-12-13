import Data.Char
import Data.List.Split
import Data.Maybe

runner i f = readFile i >>= print . f . chunksOf 6 . map read . wordsBy (not . isDigit)

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part1 = sum . mapMaybe compute1

compute1 :: [Int] -> Maybe Int
compute1 [xa, ya, xb, yb, px, py]
  | null cands = Nothing
  | otherwise  = Just $ minimum cands
  where
    cands = [ 3 * i + j
            | i <- [0 .. min (div px xa) (div py ya)]
            , let (j, r) = divMod (px - xa * i) xb, r == 0
            , ya * i + yb * j == py
            ]

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

part2x = sum . mapMaybe (compute1 . offset)

offset args = as ++ map (10000000000000 +) bs
  where
    (as,bs) = splitAt 4 args

part2 = sum . mapMaybe compute2

compute2 :: [Int] -> Maybe Int
compute2 args@[xa, ya, xb, yb, px0, py0]
  | det == 0, px == py = error $ show args -- 緊急事態、要対応
  | det == 0           = Nothing           -- 解なし
  | isOK               = Just $ 3 * i + j  -- 唯一解
  | otherwise          = Nothing           -- 自然数解でない
  where
    px = px0 + 10000000000000
    py = py0 + 10000000000000
    det = xb * ya - yb * xa
    (j, r) = divMod (ya * px - xa * py) det
    (i, s) = divMod (px - xb * j) xa
    isOK = r == 0 && s == 0 && i >= 0 && j >= 0

{-
ghci> test1
480
ghci> main1
29711
ghci> runner "sample.txt" part2x
Interrupted.
ghci> test2
875318608908
ghci> main2
94955433618919
-}
