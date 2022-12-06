-- 2022-12-3
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Data.List
import Data.List.Split

main1 = body "input.txt" compute1
test1 = body "test.txt" compute1

body fn cp = readFile fn >>= print . cp . lines

compute1 :: [String] -> Int
compute1 = sum . map func1

func1 :: String -> Int
func1 xs = prio $ head $ intersect as bs
  where
    (as,bs) = splitAt (div (length xs) 2) xs

prio c
  | c <= 'Z'  = fromEnum c - fromEnum 'A' + 27
  | otherwise = fromEnum c - fromEnum 'a' + 1

test2 = body "test.txt" compute2
main2 = body "input.txt" compute2

compute2 :: [String] -> Int
compute2 = sum . map func2 . chunksOf 3

func2 :: [String] -> Int
func2 = prio . head . foldl1 intersect
