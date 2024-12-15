import Data.List

runner i f = do
  xss <- map (map read . words) . lines <$> readFile i
  print $ f xss

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

part1 :: [[Int]] -> Int
part1 = length . filter prop1

prop1 :: [Int] -> Bool
prop1 xs = -3 <= dmin && dmax <= -1 || 1 <= dmin && dmax <= 3
  where
    (dmin, dmax) = minmaximum $ zipWith (-) xs $ tail xs

minmaximum :: Ord a => [a] -> (a, a)
minmaximum (x:xs) = loop x x xs
  where
    loop l u [] = (l,u)
    loop l u (x:xs)
      | x < l = loop x u xs
      | u < x = loop l x xs
      | True  = loop l u xs

prop1o xs = all (1 <=) ds && all (3 >=) ds || all (-3 <=) ds && all (-1 >=) ds
  where
    ds = zipWith (-) xs $ tail xs

prop2 :: [Int] -> Bool
prop2 xs = any prop1 $ xs : drop1 xs

drop1 xs = zipWith (++) (inits xs) (tail $ tails xs)

part2 :: [[Int]] -> Int
part2 = length . filter prop2

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2
