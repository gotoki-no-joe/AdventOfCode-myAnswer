import Data.Array
import qualified Data.Set as S

part12 fn body = do
  ls <- lines <$> readFile fn
  print $ body ls

solve1 cnt ls = S.size ende
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    field = listArray bnds [c /= '#' | l <- ls, c <- l]

    start = head [(i,j) | (i,l) <- zip [1..] ls, (j,'S') <- zip [1..] l]

    ende = iterate step (S.singleton start) !! cnt

    step s = S.fromList
      [ ij1
      | (i,j) <- S.elems s
      , ij1 <- [(pred i, j), (succ i, j), (i, pred j), (i, succ j)]
      , inRange bnds ij1
      , field ! ij1
      ]

{-
ghci> part12 "sample.txt" (solve1 6)
16
ghci> part12 "input.txt" (solve1 64) 
3660

パート2、何か方策ある？ひたすらやるのはかなり大きい。
-}

solve2 cnt ls = S.size ende
  where
    h = length ls
    w = length $ head ls
    bnds = ((0,0),(pred h, pred w))
    field = listArray bnds [c /= '#' | l <- ls, c <- l]

    start = head [(i,j) | (i,l) <- zip [0..] ls, (j,'S') <- zip [0..] l]

    ende = iterate step (S.singleton start) !! cnt

    step s = S.fromList
      [ ij1
      | (i,j) <- S.elems s
      , ij1@(i1,j1) <- [(pred i, j), (succ i, j), (i, pred j), (i, succ j)]
      , field ! (mod i1 h, mod j1 w)
      ]

{-
naive solution
ghci> part12 "sample.txt" (solve2 6)
16
ghci> part12 "sample.txt" (solve2 10)
50
ghci> part12 "sample.txt" (solve2 50)
1594
ghci> part12 "sample.txt" (solve2 100)
6536
ghci> part12 "sample.txt" (solve2 500)
167004
ここでインタプリタでは刺さった。コンパイルしても所詮その辺で限界だ。
-}

main = do
  part12 "sample.txt" (solve2 1000)
  part12 "sample.txt" (solve2 5000)
  part12 "input.txt" (solve2 26501365)

{-
> ghc -O2 ver1.hs
Loaded package environment from C:\Users\ohkubo\AppData\Roaming\ghc\x86_64-mingw32-9.4.7\environments\default
[1 of 2] Compiling Main             ( ver1.hs, ver1.o )
[2 of 2] Linking ver1.exe
> ./ver1 
668697

1000, 5000でこれで、26501365は無理だよな。
-}
