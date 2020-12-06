import Data.List.Split
-- import Data.List

phase1 = do
  co <- readFile "input.txt"
--  print $ sum $ map (comp1 . concat) $ wordsBy null $ lines co
  print $ sum $ map comp1 $ wordsBy null $ lines co

-- comp1 :: String -> Int
-- comp1 str = length $ group $ sort str
comp1 :: [String] -> Int
comp1 strs = length [ c | c <- ['a'..'z'], any (elem c) strs ]

phase2 = do
  co <- readFile "input.txt"
  print $ sum $ map comp2 $ wordsBy null $ lines co

comp2 :: [String] -> Int
comp2 strs = length [ c | c <- ['a'..'z'], all (elem c) strs ]

{-
*Main> phase1
6585
*Main> phase2
3276
-}
