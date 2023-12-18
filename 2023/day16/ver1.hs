import qualified Data.Set as S
import Data.Array

part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve1 ls

solve1 ls = S.size $ S.fromList $ map fst $ S.elems s
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    marr = listArray bnds $ concat ls

    s = loop S.empty [((1,1),(0,1))]
    loop s [] = s
    loop s ((ij,dir):rest)
      | not $ inRange bnds ij = loop s rest
      | S.member p s = loop s rest
      | otherwise = loop s1 r2
      where
        p = (ij, d2i dir)
        s1 = S.insert p s
        r2 = [(add ij d2, d2) | d2 <- dc dir (marr ! ij)] ++ rest

add (i,j) (x,y) = (i+x,j+y)

dc (0, 1) '|' = [(1,0),(-1,0)]
dc (0, 1) '/' =       [(-1,0)]
dc (0, 1) '\\' = [(1,0)]
dc (0,-1) '|' = [(1,0),(-1,0)]
dc (0,-1) '/' = [(1,0)]
dc (0,-1) '\\' =       [(-1,0)]
dc ( 1,0) '-' = [(0,1),(0,-1)]
dc ( 1,0) '/' =       [(0,-1)]
dc ( 1,0) '\\' = [(0,1)]
dc (-1,0) '-' = [(0,1),(0,-1)]
dc (-1,0) '/' = [(0,1)]
dc (-1,0) '\\' =       [(0,-1)]
dc dir    _   = [dir] -- '.' と通り抜ける '|''-'

d2i (i,j) = div (j+1) 2 + div (i + 1) 2 + 2 * (1 - abs j)

{-
    larr = accumArray (||) False bnds $ dfs (1,1) (0,1) []

    dfs ij dir rest
      | not $ inRange bnds ij = rest
      | otherwise = (ij, True) :
          case dc dir (marr ! ij) of
            [d1] -> dfs (add ij d1) d1 rest
            [d1,d2] -> dfs (add ij d1) d1 $ dfs (add ij d2) d2 rest
-}

{-
意地悪な無限ループは存在しない前提で、深さ優先で調べてみるか。
sampleの時点でそんなことはなかった。ちぇ。
場所に対して向きのリストを要素にする？向きもキーにして3次元入れるにする？後者だね。

ghci> part12 "sample.txt" solve1
46
ghci> part12 "input.txt" solve1 
7199

mutable arrayで高速化した版を書くべきだね。
-}

part12 fn body = do
  ls <- lines <$> readFile fn
  print $ body ls

solve2 ls = maximum [S.size $ S.fromList $ map fst $ S.elems $ loop S.empty [r] | r <- inis]
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    marr = listArray bnds $ concat ls

    inis = [r | j <- [1..w], r <- [((1,j),(1,0)),((h,j),(-1,0))]] ++
           [r | i <- [1..h], r <- [((i,1),(0,1)),((i,w),(0,-1))]]

    s = loop S.empty [((1,1),(0,1))]
    loop s [] = s
    loop s ((ij,dir):rest)
      | not $ inRange bnds ij = loop s rest
      | S.member p s = loop s rest
      | otherwise = loop s1 r2
      where
        p = (ij, d2i dir)
        s1 = S.insert p s
        r2 = [(add ij d2, d2) | d2 <- dc dir (marr ! ij)] ++ rest

{-
ghci> part12 "sample.txt" solve2
51
ghci> part12 "input.txt" solve2 
7438
-}
