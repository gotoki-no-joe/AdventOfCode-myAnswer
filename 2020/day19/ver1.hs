import Data.Array

sample = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"

parse txt = (ra,ms)
  where
    (rs,_:ms) = span (not.null) $ lines txt
    ra = array (0,length rs-1) $ map parseRule rs

data Rule = Leaf Char | Node [[Int]] deriving Show

parseRule :: String -> (Int,Rule)
parseRule txt = (read idx, doRHS rhs)
  where
    (idx,':':' ':rhs) = span (':' /=) txt
    doRHS ('\"':c:'\"':[]) = Leaf c
    doRHS rhs = Node $ loop rhs
    loop "" = []
    loop txt = map read (words r1) : loop (drop 1 rs)
      where
        (r1, rs) = span ('|' /=) txt

testRule ra str = elem "" $ inner 0 str
  where
    inner :: Int -> String -> [String]
    inner i str = case ra ! i of
        Leaf x -> [tail str | not (null str), head str == x]
        Node iss -> [r | is <- iss, r <- run is str]
    run :: [Int] -> String -> [String]
    run [] str = [str]
    run (_:_) "" = []
    run (i:is) str = inner i str >>= run is

comp1 str = length $ filter (testRule ra) ms
  where
    (ra,ms) = parse str

test1 = comp1 sample

ans1 = readFile "input.txt" >>= print . comp1

-- ループあると、これだとハングるのかな。

comp2 str = length $ filter (testRule ra1) ms
  where
    (ra,ms) = parse str
    ra1 = ra // (map parseRule ["8: 42 | 42 8","11: 42 31 | 42 11 31"])

ans2 = readFile "input.txt" >>= print . comp2

{-
*Main> test1
2
*Main> ans1
285
*Main> ans2
412
-}
