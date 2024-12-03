import qualified Data.IntSet as IS

runner i f = do
  xss <- map (map read . words) . lines <$> readFile i
  let ans = f xss
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

part1 :: [[Int]] -> Int
part1 xss = length $ filter prop1 xss

prop1 xs = IS.isSubsetOf is sp || IS.isSubsetOf is sn
  where
    is = IS.fromList $ zipWith (-) xs $ tail xs

sp, sn :: IS.IntSet
sp = IS.fromList [1 .. 3]
sn = IS.fromList [-3 .. -1]

part2 :: [[Int]] -> Int
part2 xss = length $ filter prop2 xss

prop2 xs = any prop1 $ delete1 xs

delete1 :: [a] -> [[a]]
delete1 [] = [[]]
delete1 (x:xs) = xs : map (x :) (delete1 xs)

{-
ghci> test1
2
ghci> main1
282
ghci> test2
4
ghci> main2
349
-}
