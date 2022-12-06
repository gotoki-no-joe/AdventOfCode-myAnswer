-- 2022-11-19

import qualified Data.IntMap as IM

part1 xs = IM.findWithDefault 0 150 im
  where
    im = foldl step (IM.singleton 0 1) xs
    step im x = IM.unionWith (+) im $ IM.mapKeysMonotonic (x +) im

main1 = readFile "input.txt" >>= print . part1 . map read . lines

part2 xs = (length as, b)
  where
    im = foldl step (IM.singleton 0 [1]) xs
    step im x = IM.unionWith (zipWith1 (+)) im $ IM.mapKeysMonotonic (x +) $ IM.map (0 :) im
    (as,b:_) = span (0 ==) $ IM.findWithDefault [] 150 im

zipWith1 :: (a->a->a) -> [a] -> [a] -> [a]
zipWith1 _ xs [] = xs
zipWith1 _ [] ys = ys
zipWith1 f (x:xs) (y:ys) = f x y : zipWith1 f xs ys

main2 = readFile "input.txt" >>= print . part2 . map read . lines
