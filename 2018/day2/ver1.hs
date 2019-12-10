import Data.List

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ps = map id2kind ls
  let twos = length $ filter fst ps
  let thrs = length $ filter snd ps
  print $ twos*thrs
  print $ findid ls

-- 同じ文字の出現回数を数え、2と3がそこに現れたかのペアにする

id2kind :: String -> (Bool,Bool)
id2kind xs = (elem 2 cnt, elem 3 cnt) where
  cnt = map length $ group $ sort xs

-- part 2
-- こちらも総当たりする

findid :: [String] -> [String]
findid xss = -- head
  [ common xs ys
  | (xs:xss1) <- tails xss
  , ys <- xss1
  , distance1 xs ys
  ]

distance1 :: String -> String -> Bool
distance1 xs ys = singleton $ filter id $ zipWith (/=) xs ys

singleton :: [a] -> Bool
singleton [_] = True
singleton _ = False

common :: String -> String -> String
common xs ys = map fst $ filter (uncurry (==)) $ zip xs ys

{-
*Main> main
7221
["mkcdflathzwsvjxrevymbdpoq"]
-}
