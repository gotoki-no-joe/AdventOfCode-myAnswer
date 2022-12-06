-- 2202-12-01
import Data.List
import Data.List.Split

property a b c = a + b > c && b + c > a && c + a > b

main1 =
  readFile "input.txt" >>=
  print . length . filter (\[a,b,c] -> property a b c) . map (map read . words) . lines

main2 =
  readFile "input.txt" >>=
  pure . concat . map transpose . chunksOf 3 . map (map read . words) . lines >>=
  print . length . filter (\[a,b,c] -> property a b c)
