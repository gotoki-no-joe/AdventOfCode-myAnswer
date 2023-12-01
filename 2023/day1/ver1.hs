import Data.Char
import Data.List

test1 = do
    ls <- readFile "sample.txt"
    let ans = solve1 ls
    print ans

part1 = do
    ls <- readFile "input.txt"
    let ans = solve1 ls
    print ans

solve1 :: String -> Int
solve1 = sum . map f . lines
  where
    f l = read [head ds, last ds]
      where
        ds = filter isDigit l

solve2 = sum . map f . lines
  where
    f l = read [head ds, last ds]
      where
        ds = loop l
    loop "" = []
    loop (c:xs) | isDigit c = c : loop xs
    loop xs
      | null cands = loop (tail xs)
      | otherwise  = head cands
      where
--      cands = [c : loop (drop (length d) xs) | (d,c) <- zip digits ['1'..'9'], isPrefixOf d xs]
        cands = [c : loop (tail            xs) | (d,c) <- zip digits ['1'..'9'], isPrefixOf d xs]

digits = ["one","two","three","four","five","six","seven","eight","nine"]

test2 = do
    ls <- readFile "sample2.txt"
    let ans = solve2 ls
    print ans

part2 = do
    ls <- readFile "input.txt"
    let ans = solve2 ls
    print ans

{-
ghci> part2
54412
your answer is too low.

例2の4行めに twone があったので、そういうのアリなのねと。
-}
