import Data.List
part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve1 ls

solve1 ls = sum [dist p q | p:qs <- tails xys, q <- qs]
  where
    xys = [(x,y) | (x, l) <- zip [0..] ls, (y, '#') <- zip [0..] l]
    xgaps = [x | (x, l) <- zip [0..] ls, all ('.' ==) l]
    ygaps = [y | (y, l) <- zip [0..] (transpose ls), all ('.' ==) l]
    dist (x,y) (z,w) = abs (x - z) + abs (y - w) +
                       length (takeWhile (max x z >) $ dropWhile (min x z >=) xgaps) +
                       length (takeWhile (max y w >) $ dropWhile (min y w >=) ygaps)

{-
#が一つも含まれない行を2行と数える。
#ごとのマンハッタン距離を数えて、総和をとる。
-}

part2 fn mag = do
  ls <- lines <$> readFile fn
  print $ solve2 mag ls

solve2 mag ls = sum [dist p q | p:qs <- tails xys, q <- qs]
  where
    xys = [(x,y) | (x, l) <- zip [0..] ls, (y, '#') <- zip [0..] l]
    xgaps = [x | (x, l) <- zip [0..] ls, all ('.' ==) l]
    ygaps = [y | (y, l) <- zip [0..] (transpose ls), all ('.' ==) l]
    dist (x,y) (z,w) = abs (x - z) + abs (y - w) +
                       mag * length (takeWhile (max x z >) $ dropWhile (min x z >=) xgaps) +
                       mag * length (takeWhile (max y w >) $ dropWhile (min y w >=) ygaps)

{-
ghci> part2 "samp1.txt" 1 
374
ghci> part2 "samp1.txt" 9
1030
ghci> part2 "samp1.txt" 99
8410
ghci> part2 "input.txt" 1 
9795148
ghci> part2 "input.txt" 999999
650672493820

無茶無茶簡単じゃねぇの。
-}
