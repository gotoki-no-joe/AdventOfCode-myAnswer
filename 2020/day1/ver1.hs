import qualified Data.IntSet as IS
import Data.List

phase1 = do
    cont <- readFile "input.txt"
    let xs = map read $ lines cont
    print $ compute1 xs

compute1 :: [Int] -> Int
compute1 xs = head [ x * y | x <- xs, let y = 2020 - x, IS.member y s ]
  where
    s = IS.fromList xs

{-
足し合わせると2020になる2つの値を見つけ、その積を返す

200エントリなので、線形に探しても全然平気。でもmodule使うか。
-}

phase2 = do
    cont <- readFile "input.txt"
    let xs = map read $ lines cont
    print $ compute2 xs

compute2 :: [Int] -> Int
compute2 xs = head [ x * y * z| (x:ys) <- tails xs, y <- ys, let z = 2020 - x - y, IS.member z s ]
  where
    s = IS.fromList xs

{-
*Main> phase1
1014624
*Main> phase2
80072256
-}