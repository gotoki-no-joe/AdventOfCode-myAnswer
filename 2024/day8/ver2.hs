import qualified Data.Map as M

import Data.List
import qualified Data.Set as S
import Data.Ix

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      m = buildMap ls
  print $ f h w m

buildMap :: [String] -> M.Map Char [(Int,Int)]
buildMap ls = M.fromListWith (++) [(c,[(i,j)]) | (i, l) <- zip [1 ..] ls, (j, c) <- zip [1 ..] l, c /= '.']

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Int -> Int -> M.Map Char [(Int,Int)] -> Int
part1 h w m = S.size antinodes
  where
    antinodes = S.fromList
      [ anti
      | ps <- M.elems m
      , (a,b):cds <- tails ps, (c,d) <- cds
      , anti <- [(c+c-a,d+d-b), (a+a-c,b+b-d)]
      , inRange ((1,1),(h,w)) anti
      ]

{-
cntGCD h w m = gcdCnt
  where
    gcdCnt = M.fromListWith (+)
      [ (gcd (abs $ a - c) (abs $ b - d), 1)
      | ps <- M.elems m
      , (a,b):cds <- tails ps, (c,d) <- cds
      ]
考えてみたら、最初から相対ベクトルをgcdで割った値で延長すれば問題ないので、
そういう場合があるかどうかを気にする必要はなかった。
-}

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Int -> Int -> M.Map Char [(Int,Int)] -> Int
part2 h w m = S.size antinodes
  where
    antinodes = S.fromList
      [ anti
      | ps <- M.elems m
      , (a,b) <- ps, (c,d) <- ps, (a,b) /= (c,d)
      , anti <- takeWhile (inRange ((1,1),(h,w))) $ zip [a, c ..] [b, d ..]
      ]
