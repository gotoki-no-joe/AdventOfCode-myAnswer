runner i f = do
  nss <- map (map read . words). lines <$> readFile i
  print $ f nss

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: [[Int]] -> Int
part1 nss = sum $ map loop nss
  where
    loop ns
      | all (0 ==) ns = 0
      | otherwise = last ns + loop (zipWith (-) (tail ns) ns)

{-
ghci> test1
114
ghci> main1
1789635132
-}

part2 nss = sum $ map loop nss
  where
    loop ns
      | all (0 ==) ns = 0
      | otherwise = head ns - loop (zipWith (-) (tail ns) ns)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
2
ghci> main2
913
-}
