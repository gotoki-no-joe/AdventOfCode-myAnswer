import Data.List.Split
import Data.List

main = do
  fi <- readFile "input.txt"
  let ls = lines fi
  let nss = map (map read.splitOn "\t") ls :: [[Int]]
  let ans1 = sum $ map (\row -> maximum row - minimum row) nss
  print ans1
  let ans2 = sum $ map compute nss
  print ans2

{-
前半は minimumとmaximumで簡単だった。

後半は、総当たりでするしかないのかな。
-}

compute ns = head
  [ a
  | (p:qs) <- tails ns
  , q <- qs
  , let (a,b) = divMod (max p q) (min p q)
  , b == 0
  ]

{-
*Main> main
42378
246
-}
