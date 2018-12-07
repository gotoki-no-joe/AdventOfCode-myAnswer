import Data.List.Split
import Data.List

main = do
  fi <- readFile "input.txt"
  let nss = map (map read) $ map words $ lines fi
  let ans1 = compute1 nss
  print ans1
  putStrLn "part2"
  let nss' = concat $ map transpose $ chunksOf 3 nss
  let ans2 = compute1 nss'
  print ans2

compute1 nss = length $ filter validTriangle nss

validTriangle :: [Int] -> Bool
validTriangle [a,b,c] = a+b > c && b+c > a && c+a > b

{-
*Main> main
1032
part2
1838
-}
