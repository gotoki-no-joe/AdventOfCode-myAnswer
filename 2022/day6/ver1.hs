-- 2022-12-6

import Data.List

phase1 :: String -> Int
phase1 = detect 4
-- (4 +) . length . takeWhile id . map ((4 >) . length . nub . take 4) . tails

test1 = phase1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 7
test2 = phase1 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 5
test3 = phase1 "nppdvjthqldpwncqszvftbrmjlhg" == 6
test4 = phase1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 10
test5 = phase1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 11

main1 = readFile "input.txt" >>= print . phase1

detect :: Int -> String -> Int
detect k = (k +) . length . takeWhile id . map ((k >) . length . nub . take k) . tails

phase2 = detect 14

test6 = phase2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" == 19
test7 = phase2 "bvwbjplbgvbhsrlpgdmjqwftvncz" == 23
test8 = phase2 "nppdvjthqldpwncqszvftbrmjlhg" == 23
test9 = phase2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" == 29
test10 = phase2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" == 26

main2 = readFile "input.txt" >>= print . phase2
