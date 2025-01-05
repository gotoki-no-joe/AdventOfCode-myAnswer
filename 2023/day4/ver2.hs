import Data.List
import Data.Array

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

parse :: String -> ([Int], [Int])
parse l = (map read $ words l2, map read $ words l3)
  where
    _:l1 = dropWhile (':' /=) l
    (l2,_:l3) = break ('|' ==) l1

runner i f = do
  cards <- map parse . lines <$> readFile i
  print $ f cards

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 cards = sum $ map score cards

score (xs, ys)
  | cnt == 0 = 0
  | otherwise = 2 ^ pred cnt
  where
    cnt = length $ filter (flip elem xs) ys

{-
ghci> test1
13
ghci> main1
21138
-}

part2 cards = sum $ elems arr
  where
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    u = length cards
    arr = listArray (1, u)
      [ succ $ sum $ map (arr !) [succ i .. min u $ i + m]
      | (i, m) <- zip [1 ..] wins ]

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

{-
ghci> test2
30
ghci> main2
7185540
-}

part2a cards = sum nsN
  where
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    nsN = foldr step [] wins
    step w ns = (1 + sum (take w ns)) : ns

test2a = runner "sample.txt" part2a
main2a = runner "input.txt" part2a

part2b cards = sum ns
  where
    (_,ns) = mapAccumL step ones wins
    ones = map (const 1) cards -- 持ちクジ数初期値
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards] -- 当たり数
    step (n:ns) m = (ns3, n)
      where
        (ns1, ns2) = splitAt m ns
        ns3 = map (n +) ns1 ++ ns2

test2b = runner "sample.txt" part2b
main2b = runner "input.txt" part2b

part2c cards = runST $
  do
    ns <- newArray (1,u) 1 :: ST s (STUArray s Int Int) -- 持ちクジ数配列、初期値1
    forM_ (zip [1 ..] wins) (\(i, m) -> do       -- クジ1から順に、1枚あたりの当たり数mのクジiが
      k <- readArray ns i                        -- k枚あるから
      forM_ [succ i .. min u $ i + m] (\j -> do  -- クジ i+1～i+m (上限u) を
        t <- readArray ns j
        writeArray ns j $ t + k                  -- k枚ずつ増やす
        )
      )
    sum <$> getElems ns
  where
    u = length cards
    wins = [length $ filter (flip elem xs) ys | (xs,ys) <- cards]

test2c = runner "sample.txt" part2c
main2c = runner "input.txt" part2c
