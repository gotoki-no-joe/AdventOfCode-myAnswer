{-
正規表現ないと辛いぞ。
-}

import Data.List
import Data.Char
import Data.Ix

runner i f = do
  s <- readFile i
  let ans = f s
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

test2 = runner "sample2.txt" part2
main2 = runner "input.txt" part2

part1 :: String -> Int
part1 s = sum $ map p1sub $ tails s

p1sub :: String -> Int
p1sub s
  | c1, c2, c3, c4, c5 = read s1 * read s3
  | otherwise = 0
  where
    c1 = isPrefixOf "mul(" s
    (s1, s2) = span isDigit $ drop 4 s
    c2 = inRange (1,3) $ length s1
    c3 = not (null s2) && head s2 == ','
    (s3,s4) = span isDigit $ tail s2
    c4 = inRange (1,3) $ length s3
    c5 = not (null s4) && head s4 == ')'

part2 :: String -> Int
part2 s = sum $ loop True $ map p2sub $ tails s
  where
    loop _ [] = []
    loop True  (Right x : cs) = x : loop True  cs
    loop False (Right _ : cs) =     loop False cs
    loop _ (Left b : cs) = loop b cs

p2sub :: String -> Either Bool Int
p2sub s
  | isPrefixOf "don't()" s = Left False
  | isPrefixOf "do()" s = Left True
  | c1, c2, c3, c4, c5 = Right $ read s1 * read s3
  | otherwise = Right 0
  where
    c1 = isPrefixOf "mul(" s
    (s1, s2) = span isDigit $ drop 4 s
    c2 = inRange (1,3) $ length s1
    c3 = not (null s2) && head s2 == ','
    (s3,s4) = span isDigit $ tail s2
    c4 = inRange (1,3) $ length s3
    c5 = not (null s4) && head s4 == ')'

{-
読み込みに成功した文字列も飛ばさずに1文字ずらしで全部調べる富豪的ソリューション
-}
