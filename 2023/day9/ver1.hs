part1 fn = do
    nss <- map (map read . words). lines <$> readFile fn
    print $ solve1 nss

solve1 :: [[Int]] -> Int
solve1 nss = sum $ map single nss

single ns
  | all (0 ==) ns = 0
  | otherwise     = last ns + single ns1
  where
    ns1 = zipWith (-) (tail ns) ns

{-
ghci> part1 "sample.txt"
114
ghci> part1 "input.txt"
1789635132
-}

{-
パート2って、逆順にしてからやるだけではいかんのか？
-}

part2 fn = do
    nss <- map (map read . words). lines <$> readFile fn
    print $ solve2 nss

solve2 :: [[Int]] -> Int
solve2 nss = sum $ map (single . reverse) nss

{-
ghci> part2 "sample.txt"
2
ghci> part2 "input.txt"
913

できちゃったよ。なんだそれ。
-}
