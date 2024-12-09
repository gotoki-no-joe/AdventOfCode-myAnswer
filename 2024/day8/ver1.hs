{-
直線上にあって、距離が2倍、がカウントするものの条件。

つまり、位置(a,b)について、全てのアンテナ(c,d)に対して、
その倍の位置(c + c - a, d + d - b)に同じ文字のアンテナがある、
ようなものを数えればよい。

時間掛かりそうなので、同じ周波数のアンテナの座標を集めて、
nC2で総当たりに、問題の位置で枠内のものを集めて数える、としてみよう。
(a,b) と (c,d) があるとき、(c + c - a, d + d - b) と (a + a - c, b + b - d) がそれ。
-}

import Data.Ix
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1
main1 = runner "input.txt"  part1

test2 = runner "sample.txt" part2
main2 = runner "input.txt"  part2

part1 ls = S.size antinodes
  where
    h = length ls
    w = length $ head ls
--    grid = listArray ((1,1),(h,w)) $ concat ls
    antennaMap = M.fromListWith (++) [(c,[(i,j)]) | (i,l) <- zip [1 ..] ls, (j,c) <- zip [1..] l, c /= '.']
    antinodes = S.fromList
      [ anti
      | ps <- M.elems antennaMap
      , (a,b):cds <- tails ps, (c,d) <- cds
      , anti <- [(c+c-a,d+d-b), (a+a-c,b+b-d)]
      , inRange ((1,1),(h,w)) anti
      ]

{-
part2 の方、2 by 2 みたなgcd持つ配置のとき、1 by 1 は該当するのかどうか、それがわからない。
では全ての組み合わせの、座標の差のgcdを計算してみよう。

全部1でしたので、part1の延長でいける。
-}

cntGCD ls = antinodes
  where
    h = length ls
    w = length $ head ls
    antennaMap = M.fromListWith (++) [(c,[(i,j)]) | (i,l) <- zip [1 ..] ls, (j,c) <- zip [1..] l, c /= '.']
    antinodes = M.fromListWith (+)
      [ (gcd (abs $ a - c) (abs $ b - d), 1)
      | ps <- M.elems antennaMap
      , (a,b):cds <- tails ps, (c,d) <- cds
      ]

part2 ls = S.size antinodes
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    antennaMap = M.fromListWith (++) [(c,[(i,j)]) | (i,l) <- zip [1 ..] ls, (j,c) <- zip [1..] l, c /= '.']
    antinodes = S.fromList $ concat
      [ takeWhile (inRange bnds) $ zip [a,c ..] [b,d ..]
      | ps <- M.elems antennaMap
      , (a,b) <- ps, (c,d) <- delete (a,b) ps
      ]

{-
ghci> test2
34
ghci> main2
1352
-}
