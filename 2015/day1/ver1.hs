-- part 1

main = do
  l <- readFile "input.txt"
  let ans1 = compute l
  print ans1

-- paren to function
-- 開き括弧は階を増やし
-- 閉じ括弧は減らす
p2f '(' = succ
p2f ')' = pred

-- 0を初期値として全て適用した最終結果を求めればよい
compute :: String -> Int
compute l = foldl (flip ($)) 0 $ map p2f l

{-
*Main> main
74
-}

-- part 2

main2 = do
  l <- readFile "input.txt"
  let ans2 = compute2 l
  print ans2

-- scanlはfoldlの中間結果のリストを返す
-- 最初に-1になるまでの数の個数が求める値
compute2 l = length $ takeWhile (-1 <) $ scanl (flip ($)) 0 $ map p2f l

{-
*Main> main2
1795
-}