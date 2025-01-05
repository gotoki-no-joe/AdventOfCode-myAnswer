import Data.List

runner i f = do
  ls <- lines <$> readFile i
  print $ f ls

test1 = runner "sample.txt" $ part12 2
main1 = runner "input.txt"  $ part12 2
test2 = runner "sample.txt" $ part12 10
test3 = runner "sample.txt" $ part12 100
main2 = runner "input.txt"  $ part12 1000000

part12 mag ls = sum [dist p q | p:qs <- tails xys, q <- qs]
  where
-- 星の位置
    xys = [(x,y) | (x, l) <- zip [0..] ls, (y, '#') <- zip [0..] l]
-- 倍数えるべき行と列の位置
    xgaps = [x | (x, l) <- zip [0..] ls, all ('.' ==) l]
    ygaps = [y | (y, l) <- zip [0..] (transpose ls), all ('.' ==) l]
-- マンハッタン距離
    dist (x,y) (z,w) = abs (x - z) + abs (y - w) + pred mag * (gx + gy)
      where
        gx = length (takeWhile (max x z >) $ dropWhile (min x z >=) xgaps)
        gy = length (takeWhile (max y w >) $ dropWhile (min y w >=) ygaps)

{-
ghci> test1
374
ghci> main1
9795148
ghci> test2
1030
ghci> test3
8410
ghci> main2
650672493820
-}
