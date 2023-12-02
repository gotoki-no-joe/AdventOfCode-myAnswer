{-
読み込みが面倒だぞこれ。
-}

import Data.List.Split

part1 fn = do
  dat <- map parse . lines <$> readFile fn
--  print dat
  print $ solve dat

type Record = (Int, [(Int,Int,Int)]) -- RGB

parse :: String -> Record
parse l = (cnt, map doSet sets)
  where
    (as, _:bs) = span (':' /=) l
    cnt = read $ words as !! 1
    sets = splitOn ";" bs
    doSet s = foldr (f . words) (0,0,0) (splitOn "," s)
    f [ds, 'r':_] = \(r,g,b) -> (read ds + r, g, b)
    f [ds, 'g':_] = \(r,g,b) -> (r, read ds + g, b)
    f [ds, 'b':_] = \(r,g,b) -> (r, g, read ds + b)

solve :: [Record] -> Int
solve = sum . map fst . filter (all isOK . snd)
  where
    isOK (r,g,b) = r <= 12 && g <= 13 && b <= 14

{-
ghci> part1 "sample.txt"
8
ghci> part1 "input.txt"
2268
-}

part2 fn = do
  dat <- map parse . lines <$> readFile fn
  print $ solve2 dat

solve2 :: [Record] -> Int
solve2 = sum . map (g . foldr f (0,0,0) . snd)
  where
    f (r,g,b) (x,y,z) = (max r x, max g y, max b z)
    g (r,g,b) = r * g * b

{-
ghci> part2 "sample.txt"
2286
ghci> part2 "input.txt" 
63542

3タプルでなくてリストでやるべきだったね。
そうすれば zipWith max とか product とかで済ませられた。
-}
