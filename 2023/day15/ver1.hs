import Data.Char
import Data.List.Split
import qualified Data.IntMap as IM

hash :: String -> Int
hash = foldl step 0
  where
    step :: Int -> Char -> Int
    step val c = mod ((val + ord c) * 17) 256

test1 = hash "HASH" == 52

part1 fn = readFile fn >>= print . sum . map hash . wordsBy (',' ==) . head . lines

{-
ghci> part1 "sample.txt"
1320
ghci> part1 "input.txt"
508552
-}

part2 fn = readFile fn >>= print . solve2 . wordsBy (',' ==) . head . lines

solve2 ls = power m
  where
    m = foldl step IM.empty ls
    step :: IM.IntMap [(String, Int)] -> String -> IM.IntMap [(String, Int)]
    step m l
      | b == '-'  = m1
      | null ps2  = IM.insert i ((as, n) : ps) m
      | otherwise = IM.insert i (ps1 ++ (as, n) : tail ps2) m
      where
        (as, b:cs) = span isAlpha l
        i = hash as
        ps = IM.findWithDefault [] i m
        m1 = IM.insert i (filter ((as /=) . fst) ps) m
        (ps1,ps2) = span ((as /=) . fst) ps
        n = read cs
    power m = sum
      [ x * succ i
      | (i, ps) <- IM.assocs m
      , let x = sum $ zipWith (*) [1..] $ reverse $ map snd ps
      ]

{-
ghci> part2 "sample.txt"
145
ghci> part2 "input.txt" 
265462

なんか手応えのない、やるだけ問題だのお。
-}
