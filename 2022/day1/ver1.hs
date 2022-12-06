-- 2022-12-1

import Data.List.Split
import Data.List

main12 = do
  co <- readFile "input.txt"
  let xs = map (sum . map read) $ wordsBy null $ lines co :: [Int]
  putStrLn "Part 1"
  print $ maximum xs
  putStrLn "Part 2"
  print $ sum $ take 3 $ sortBy (flip compare) xs

main1 =
  readFile "input.txt" >>=
  pure . map (map read) . wordsBy null . lines >>=
  print . maximum . map sum

main2 =
  readFile "input.txt" >>=
  pure . map (sum . map read) . wordsBy null . lines >>=
  print . sum . take 3 . sortBy (flip compare)
