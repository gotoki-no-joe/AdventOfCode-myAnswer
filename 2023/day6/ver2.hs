runner i f = do
  (ts:ds:_) <- map (map read . tail . words) . lines <$> readFile i
  print $ f ts ds

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [Int] -> [Int] -> Int
part1 ts ds = product $ zipWith f ts ds
  where
    f tee dee = length [() | t <- [0 .. tee], t * (tee - t) > dee]

{-
ghci> test1
288
ghci> main1
500346
-}

part2 :: [Int] -> [Int] -> Int
part2 ts ds = length [() | t <- [0 .. tee], t * (tee - t) > dee]
  where
    tee = read $ concatMap show ts
    dee = read $ concatMap show ds

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
71503
ghci> main2
42515755
-}

part2a :: [Int] -> [Int] -> Int
part2a ts ds = f1 - f2 + 1
  where
    t = read $ concatMap show ts
    d = read $ concatMap show ds
    sq = sqrt (t * t - 4 * d)
    f1 = floor   ((t + sq) / 2)
    f2 = ceiling ((t - sq) / 2)

test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a

{-
ghci> test2a
71503
ghci> main2a
42515755
-}
