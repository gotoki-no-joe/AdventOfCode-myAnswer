import Data.Array

sample :: [Int]
sample = [3,4,3,1,2]

{-
k日、という魚の数を持つ配列 Array (0,8) Int を使ってステップするってことだな。
リストで充分か？
-}

type State = Array Int Int

step :: State -> State
step arr = listArray (0,8) [arr ! 1, arr ! 2, arr ! 3, arr ! 4, arr ! 5, arr ! 6, arr ! 7 + arr ! 0, arr ! 8, arr ! 0]

initial :: [Int] -> State
initial xs = accumArray (+) 0 (0,8) [(x,1) | x <- xs]

-- test1 = sum $ elems $ (!! 18) $ iterate step (initial sample)
test1 = exec sample 18
-- test12 = sum $ elems $ (!! 80) $ iterate step (initial sample)
test12 = exec sample 80

dat :: [Int]
dat = [1,2,1,1,1,1,1,1,2,1,3,1,1,1,1,3,1,1,1,5,1,1,1,4,5,1,1,1,3,4,1,1,1,1,1,1,1,5,1,4,1,1,1,1,1,1,1,5,1,3,1,3,1,1,1,5,1,1,1,1,1,5,4,1,2,4,4,1,1,1,1,1,5,1,1,1,1,1,5,4,3,1,1,1,1,1,1,1,5,1,3,1,4,1,1,3,1,1,1,1,1,1,2,1,4,1,3,1,1,1,1,1,5,1,1,1,2,1,1,1,1,2,1,1,1,1,4,1,3,1,1,1,1,1,1,1,1,5,1,1,4,1,1,1,1,1,3,1,3,3,1,1,1,2,1,1,1,1,1,1,1,1,1,5,1,1,1,1,5,1,1,1,1,2,1,1,1,4,1,1,1,2,3,1,1,1,1,1,1,1,1,2,1,1,1,2,3,1,2,1,1,5,4,1,1,2,1,1,1,3,1,4,1,1,1,1,3,1,2,5,1,1,1,5,1,1,1,1,1,4,1,1,4,1,1,1,2,2,2,2,4,3,1,1,3,1,1,1,1,1,1,2,2,1,1,4,2,1,4,1,1,1,1,1,5,1,1,4,2,1,1,2,5,4,2,1,1,1,1,4,2,3,5,2,1,5,1,3,1,1,5,1,1,4,5,1,1,1,1,4]

-- run1 = sum $ elems $ (!! 80) $ iterate step (initial dat)
run1 = exec dat 80

exec :: [Int] -> Int -> Int
exec xs d = sum $ elems $ (!! d) $ iterate step (initial xs)

test2 = exec sample 256

run2 = exec dat 256

{-
*Main> test1
26
*Main> test12
5934
*Main> run1
390923
*Main> test2
26984457539
*Main> run2
1749945484935
前半だけなら素朴なsimulationでも答えを出せるけど、後半は無理だね。
-}

data State2 = R {d0 :: Int, d1 :: Int, d2 :: Int, d3 :: Int, d4 :: Int, d5 :: Int, d6 :: Int, d7 :: Int, d8 :: Int}

initial2 :: [Int] -> State2
initial2 xs = R a b c d e f g h i
  where
    (a:b:c:d:e:f:g:h:i:_) = elems $ accumArray (+) 0 (0,8) [(x,1) | x <- xs]

step2 :: State2 -> State2
step2 s = R (d1 s) (d2 s) (d3 s) (d4 s) (d5 s) (d6 s) (d7 s + d0 s) (d8 s) (d0 s)

total (R a b c d e f g h i) = a + b + c + d + e + f + g + h + i

exec2 :: Int -> [Int] -> Int
exec2 d = total . (!! d) . iterate step2 . initial2

run22 = exec2 256 dat
