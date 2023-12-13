import Data.List.Split
import Data.List

part1 fn = do
  ls <- lines <$> readFile fn
  print $ sum $ map single $ wordsBy null ls

single :: [String] -> Int
single ls = max (100 * proc ls) (proc $ transpose ls)
  where
    proc xs = maximum $ loop 0 [] xs
    loop k ys xxs@(x:xs)
      | and $ zipWith (==) ys xxs = k : rest
      | otherwise                 =     rest
      where
        rest = loop (succ k) (x:ys) xs
    loop _ _ [] = []

{-
空行で問題を区切る。

後半、比較して1文字だけ異なる位置」を取り出すことになるのな。
-}

part2 fn = do
  ls <- lines <$> readFile fn
  print $ sum $ map single2 $ wordsBy null ls

single2 :: [String] -> Int
single2 ls = max (100 * proc ls) (proc $ transpose ls)
  where
    proc xs = maximum $ 0 : loop 0 [] xs
    loop k ys xxs@(x:xs)
      | 1 == sum (zipWith g ys xxs) = k : rest
      | otherwise                   =     rest
      where
        f a b = if a == b then 0 else 1
        g xs ys = sum $ zipWith f xs ys
        rest = loop (succ k) (x:ys) xs
    loop _ _ [] = []
