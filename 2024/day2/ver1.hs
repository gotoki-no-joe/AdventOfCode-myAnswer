import qualified Data.IntSet as IS

runner i f = do
  xss <- map (map read . words) . lines <$> readFile (if i then "input.txt" else "sample.txt")
  let ans = f xss
  print ans

test1 = runner False part1
main1 = runner True  part1

test2 = runner False part2
main2 = runner True  part2

part1 :: [[Int]] -> Int
part1 xss = length $ filter prop xss
  where
    prop xs = IS.isSubsetOf is sp || IS.isSubsetOf is sn
      where
        is = IS.fromList $ zipWith (-) xs $ tail xs
    sp = IS.fromList [1 .. 3]
    sn = IS.fromList [-3 .. -1]

part2 xss = length $ filter prop2 xss
  where
    prop2 xs = any prop1 $ delete1 xs
    prop1 xs = IS.isSubsetOf is sp || IS.isSubsetOf is sn
      where
        is = IS.fromList $ zipWith (-) xs $ tail xs
    sp = IS.fromList [1 .. 3]
    sn = IS.fromList [-3 .. -1]

delete1 :: [a] -> [[a]]
delete1 [] = [[]]
delete1 (x:xs) = xs : map (x :) (delete1 xs)
