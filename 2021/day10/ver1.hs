import Data.Either
import Data.List

test1 = readFile "sample.txt" >>= print . compute1

compute1 :: String -> Int
compute1 = sum . map (fromLeft 0 . checker []) . lines

checker ys ('(':xs) = checker (')':ys) xs
checker ys ('[':xs) = checker (']':ys) xs
checker ys ('{':xs) = checker ('}':ys) xs
checker ys ('<':xs) = checker ('>':ys) xs
checker ys [] = Right $ foldl (\acc c -> acc * 5 + comp c) 0 ys
checker (y:ys) (x:xs) | y == x = checker ys xs
checker _ (')':_) = Left 3
checker _ (']':_) = Left 57
checker _ ('}':_) = Left 1197
checker _ ('>':_) = Left 25137
checker ys xs = error $ ys ++ " / " ++ xs
-- part2対応で書き換えてしまった。元はcorruptな行にスコアをIntで、そうでないとき0を返すだけの形だった。

comp ')' = 1
comp ']' = 2
comp '}' = 3
comp '>' = 4
comp _ = error "never happens"

run1 = readFile "input.txt" >>= print . compute1

compute2 :: String -> Int
compute2 = middle . filter (0 <) . map (fromRight 0 . checker []) . lines

middle xs = sort xs !! (length xs `div` 2)

test2 = readFile "sample.txt" >>= print . compute2

run2 = readFile "input.txt" >>= print . compute2

{-
*Main> test1
26397
*Main> run1
315693
*Main> test2
288957
*Main> run2
1870887234
-}
