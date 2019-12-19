import Data.Char

sample0 = "12345678"
sample1 = "80871224585914546619083218645595"
sample2 = "19617804207202209144916044189917"
sample3 = "69317163492948606335995924319873"

-- pattern = [0,1,0,-1]
pattern = [const 0, id, const 0, negate]

prepare = map digitToInt

step :: Int -> [Int] -> [Int]
step n list = map f [1..n]
  where
    pat k = tail $ cycle $ concatMap (replicate k) pattern
--    f k = flip mod 10 $ abs $ sum $ zipWith (*) (pat k) list
    f k = flip mod 10 $ abs $ sum $ zipWith ($) (pat k) list

test0 = take 5 $ iterate (step 8) (prepare sample0)
part1 s = take 8 $ (!! 100) $ iterate (step (length s)) (prepare s)
test1 = part1 sample1
test2 = part1 sample2
test3 = part1 sample3

main = do
  li <- readFile "input.txt"
  let ans = part1 $ init li
  putStrLn (concatMap show ans)

{-
*Main> main
63794407
-}

-- too naive to finish
part2 s = take 8 $ drop ofs $ mid
  where
    ofs = read $ take 7 s
    mid = (!! 100) $ iterate (step (10000 * length s)) $ prepare $ concat $ replicate 10000 s

test4 = part2 "03036732577212944063491565474664"
test5 = part2 "02935109699940807407585447034323"
test6 = part2 "03081770884921959731165446850517"
