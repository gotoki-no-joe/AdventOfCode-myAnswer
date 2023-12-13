{-
役を構成する主要なカードの強弱じゃなくて、昇順ソートした辞書順で大きい方だから
33332 > 2AAAA とかひどいルールだな。作りやすいけど。多分。

って、カード順ソートしないで、表示された順のママで比較すんの？訳わからんな。

-}
import Data.List
import Data.Maybe
import Data.Array

import Debug.Trace

part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve1 ls

solve1 :: [String] -> Int
solve1 ls = sum $ zipWith (*) [1..] $ map snd $ sortOn fst $ map parse ls

data TypeCP = HighCard | OnePair | TwoPair | Three1K | FullHouse | Four1K | Five1K deriving (Eq, Ord, Show)

type Hand = (TypeCP, [Int])

c2i :: Char -> Int
c2i c = fromJust $ elemIndex c "23456789TJQKA"

parse :: String -> (Hand, Int)
parse l =
  case cs of
    [5] -> ans Five1K
    [1,4] -> ans Four1K
    [2,3] -> ans FullHouse
    [1,1,3] -> ans Three1K
    [1,2,2] -> ans TwoPair
    [1,1,1,2] -> ans OnePair
    [1,1,1,1,1] -> ans HighCard
  where
    [c5, bs] = words l
    i5 = map c2i c5
    cs = sort $ filter (0 <) $ elems $ accumArray (+) 0 (0,12) [(i, 1) | i <- i5]
    ans tp = ((tp, i5), read bs)

{-
ghci> part1 "sample.txt"
6440
ghci> part1 "input.txt"
250453939
-}

{-
パート２

Jを最弱にみなして、ただし、Jokerとして好きに読み替えてよいとすると。

typeごとに、役を構成していないカードがJだったら何になれるか、を、その枚数のバリエーションも含めて考えて、
Jでないカードで最も枚数の多いものに寄せる、が正解だろうと。
-}

c2i2 :: Char -> Int
c2i2 c = fromJust $ elemIndex c "J23456789TQKA"

parse2 :: String -> (Hand, Int)
parse2 l
  | c5 == "JJJJJ" = ans Five1K
  | otherwise = -- trace (show (c0:c1:cs)) $
  case c:cs of
    [5] -> ans Five1K
    [4,1] -> ans Four1K
    [3,2] -> ans FullHouse
    [3,1,1] -> ans Three1K
    [2,2,1] -> ans TwoPair
    [2,1,1,1] -> ans OnePair
    [1,1,1,1,1] -> ans HighCard
  where
    [c5, bs] = words l
    i5 = map c2i2 c5
    biasJ = 100
    c0 : c1 : cs = sortBy (flip compare) $ filter (0 /=) $ elems $ accumArray (+) 0 (0,12) $ (0, biasJ) : [(i, 1) | i <- i5]
    c = c0 - biasJ + c1
    ans tp = ((tp, i5), read bs)

part2 fn = do
  ls <- lines <$> readFile fn
  print $ solve2 ls

solve2 :: [String] -> Int
solve2 ls = sum $ zipWith (*) [1..] $ map snd $ sortOn fst $ map parse2 ls

{-
ghci> part2 "sample.txt"
5905
ghci> part2 "input.txt"
248652697

関数に切り出す位置を間違えたね。handの文字列だけにしてからその先をデバッグしていたから、そこだけ単独にしないといけなかった。
-}
