import Data.Char
import Data.List

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

{-
part1
数字文字だけのリストを作り、先頭と末尾を取り出す
-}

part1 :: [String] -> Int
part1 = sum . map (d1d2 . getds)
  where
    d1d2 ds = head ds * 10 + last ds
    getds l = [digitToInt c | c <- l, isDigit c]

part1a ls = sum
  [ head ds * 10 + last ds
  | l <- ls
  , let ds = map digitToInt $ filter isDigit l]

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

{-
綴りの一部に重なりがあるのをどう処理するかよくわからないけど、
全ての位置から探す、でやってみる。
-}

dic :: [(String, Int)]
dic = [([intToDigit i], i) | i <- [1 .. 9]] ++
      zip (words "one two three four five six seven eight nine") [1 ..]

part2 :: [String] -> Int
part2 = sum . map (d1d2 . getds)
  where
    d1d2 ds = head ds * 10 + last ds
    getds l =
      [ d
      | tl <- tails l
      , d <- take 1 [d | (s,d) <-dic, isPrefixOf s tl] ]

part2a ls = sum
  [ head ds * 10 + last ds
  | l <- ls
  , let ds = [d | tl <- tails l, (s,d) <- dic, isPrefixOf s tl]]

test2 = runner "sample2.txt" part2
main2 = runner "input.txt" part2
