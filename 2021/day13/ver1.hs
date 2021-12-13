import qualified Data.Set as S
import Data.List
import Data.Array

type Paper = S.Set (Int,Int)
type Cmd = (Char, Int)

parse :: String -> (Paper, [Cmd])
parse xs = (S.fromList $ map parsep ps, map parsec cs)
  where
    (ps, _:cs) = span ("" /=) $ lines xs
    parsep xs = (read a, read b)
      where
        (a,_:b) = span (',' /=) xs
    parsec xs = (xs !! 11, read $ drop 13 xs)

doFold :: Cmd -> Paper -> Paper
doFold (xy,wh) p = S.map f p
  where
    wh2 = wh + wh
    g p = case compare p wh of
      LT -> p
      EQ -> error "online"
      GT -> wh2 - p
    f = if xy == 'x' then \(x,y) -> (g x, y)
                     else \(x,y) -> (x, g y)

-- compute1 :: String -> Int
compute1 xs = S.size $ doFold (head cmds) paper
  where
    (paper,cmds) = parse xs

test1 = readFile "sample.txt" >>= print . compute1
run1 = readFile "input.txt" >>= print . compute1

{-
*Main> test1
17
*Main> run1
684
-}

compute2 xs = [[arr ! (i,j) | i <- [0..w]] | j <- [0..h]]
  where
    (paper, cmds) = parse xs
    folded = foldl (flip doFold) paper cmds
    ps = S.elems folded
    w = maximum (map fst ps)
    h = maximum (map snd ps)
    arr = accumArray (flip const) '.' ((0,0),(w,h)) $ zip ps $ repeat '#'

test2 = readFile "sample.txt" >>= mapM_ putStrLn . compute2
run2 = readFile "input.txt" >>= mapM_ putStrLn . compute2

{-
*Main> test2
#####
#...#
#...#
#...#
#####
*Main> run2
..##.###..####.###..#.....##..#..#.#..#
...#.#..#....#.#..#.#....#..#.#.#..#..#
...#.#..#...#..###..#....#....##...####
...#.###...#...#..#.#....#.##.#.#..#..#
#..#.#.#..#....#..#.#....#..#.#.#..#..#
.##..#..#.####.###..####..###.#..#.#..#

JRZBLGKH
-}
