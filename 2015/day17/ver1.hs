{-
これ、ナップサック問題そのものだよね。
「残りを全部使ったらどれだけになるか」を先に数えておくことで枝刈りができる。
-}

import Data.List

main = do
  fi <- readFile "input.txt"
  let ns = reverse $ sort $ map read $ words fi
  let ls = scanr1 (+) ns
  let ans1 = compute1 ns ls 150
  print (length ans1)
  let ans2 = compute2 ans1
  print ans2
  print (length ans2)

compute1 :: [Int] -> [Int] -> Int -> [[Int]]
compute1 ns ls total = loop nls total where
  nls = zip ns ls
  loop _ 0 = [ [] ]
  loop [] _ = [ ]
  loop ((n,l):nls) w =
    case compare l w of
      LT -> []
      EQ -> [ n:map fst nls ]
      GT -> if n > w then loop nls w else map (n :) (loop nls (w-n)) ++ loop nls w

test1 = compute1 [20,15,10,5,5] (scanr1 (+) [20,15,10,5,5]) 25

compute2 :: [[Int]] -> [[Int]]
compute2 nss = [ ns | (ns, len) <- zip nss lens, len == x ] where
  lens = map length nss
  x = minimum lens

test2 = compute2 test1

{-
*Main> test1
[[20,5],[20,5],[15,10],[15,5,5]]
*Main> test2
[[20,5],[20,5],[15,10]]
*Main> main
654
[[50,49,44,7],...
57
-}
