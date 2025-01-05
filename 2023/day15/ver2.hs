import Data.List.Split
import Data.List
import Data.Array
import Data.Char

runner i f = do
  steps <- wordsBy (',' ==) . head . lines <$> readFile i
  print $ f steps

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

hash = foldl' step 0
  where
    step acc c = mod ((acc + ord c) * 17) 256

part1 = sum . map hash

{-
ghci> test1
1320
ghci> main1
508552
-}

-- 指定したラベルのレンズを除く
delLens label ls = filter ((label /=) . fst) ls

-- 指定したラベルのレンズがあるとき、焦点距離を指定のものに差し替える。
-- ないとき、末尾に挿入する
insLens label focus [] = [(label, focus)]
insLens label focus (l:ls)
  | fst l == label = (label, focus) : ls
  | otherwise = l : insLens label focus ls

part2 steps = sum
  [ sum $ zipWith (*) [i, i + i ..] $ map snd ls
  | (i, ls) <- zip [1 ..] $ elems arr ]
  where
    arr = accumArray (flip ($)) [] (0, 255) $
          map makeCmd steps
    makeCmd s
      | last s == '-' = (hash lab1, delLens lab1)
      | otherwise     = (hash lab2, insLens lab2 (digitToInt $ last s))
      where
        lab1 = init s
        lab2 = init lab1

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
145
ghci> main2
265462
-}
