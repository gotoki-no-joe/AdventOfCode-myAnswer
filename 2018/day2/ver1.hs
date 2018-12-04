import Data.List (tails)

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let ps = map id2kind ls
  let twos = length $ filter fst ps
  let thrs = length $ filter snd ps
  print $ twos*thrs
  print $ findid ls

{-
IDは大した長さではないから、数えるのが面倒なのが問題だ。
適当にやるか。
-}

id2kind :: String -> (Bool,Bool)
id2kind xs = (elem 2 cnt, elem 3 cnt) where
  cnt = [ length $ filter (x ==) xs | x <- xs ]

{-
後半も総当たりするしかないのか？
-}

-- findid :: [String] -> String
findid xss = -- head
  [ common xs ys
  | (xs:xss1) <- tails xss
  , ys <- xss1
  , distance xs ys == 1
  ]

distance :: String -> String -> Int
distance xs ys = length $ filter id $ zipWith (/=) xs ys

common :: String -> String -> String
common xs ys = map fst $ filter (uncurry (==)) $ zip xs ys

{-
*Main> main
7221
["mkcdflathzwsvjxrevymbdpoq"]
-}
