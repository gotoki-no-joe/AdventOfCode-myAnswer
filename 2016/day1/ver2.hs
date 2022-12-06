-- 2022-11-29

import Data.Char
import qualified Data.Set as S
import Data.List

parse :: String -> [Either Int Int]
parse l = loop l
  where
    loop ('L':xs) = sub Left  xs
    loop ('R':xs) = sub Right xs
    loop "" = []
    sub f xs = let (as,bs) = span isDigit xs in f (read as) : loop (dropWhile (not.isUpper) bs)

step ((x,y), (dx,dy)) (Left  n) = ((x - n * dy, y + n * dx), (- dy, dx))
step ((x,y), (dx,dy)) (Right n) = ((x + n * dy, y - n * dx), (dy, - dx))

main1 = do
  co <- readFile "input.txt"
  let is = parse co
  let ((x,y),_) = foldl step ((0,0), (0,1)) is
  print $ abs x + abs y
  let (z,w) = loop S.empty $ positions $ deltas1 is
  print $ abs z + abs w

deltas :: [Either Int Int] -> [(Int, Int)]
deltas = concat . snd . mapAccumL step (0,1)
  where
    step (dx,dy) instr = (dxy1, replicate n dxy1)
      where
        (dxy1, n) = case instr of
          Left  n -> ((- dy, dx), n)
          Right n -> ((dy, - dx), n)

deltas1 :: [Either Int Int] -> [(Int, Int)]
deltas1 = concat . snd . mapAccumL step (0,1)
  where
    step (dx,dy) (Left  n) = sub n (- dy, dx)
    step (dx,dy) (Right n) = sub n (dy, - dx)
    sub n dxy = (dxy, replicate n dxy)

positions :: [(Int,Int)] -> [(Int,Int)]
positions = scanl add (0,0)
  where
    add (x, y) (dx, dy) = (x + dx, y + dy)

loop s (p:ps)
  | S.member p s = p
  | otherwise    = loop (S.insert p s) ps
