{-
とりあえず縁取りして、配列に入れて、
数字の塊を探して、その周囲を確認してみようか。
-}
import Debug.Trace

import Data.Array
import Data.Char

part1 fn = do
  ls <- lines <$> readFile fn
  let ans = solve ls
  print ans

solve ls = sum
    [ read [ca ! (i, j1) | j1 <- [j .. k]]
    | (i,j) <- range((1,1),(h,w))
    , not (isDigit (ca ! (i,pred j))), isDigit (ca ! (i,j))
    , let k = last $ takeWhile (\j -> isDigit $ ca ! (i,j)) [j..]
    , any (isTheSymbol . (ca !)) $ (i, pred j) : (i, succ k) : [(i1, j1) | i1 <- [pred i, succ i], j1 <- [pred j .. succ k]]
    ]
  where
    h = length ls
    w = length $ head ls
    ca = accumArray (flip const) '.' ((0,0),(succ h, succ w))
         [ ((i,j), c) | (i,l) <- zip [1..] ls, (j,c) <- zip [1..] l]

isTheSymbol c = c /= '.' && not (isDigit c)

{-
ghci> part1 "sample.txt"
4361
ghci> part1 "input.txt"
526404
-}

-- * だけを気にして、数字列がそれに隣接しているもののみ、*の位置に対して自分の値を登録する。
-- この結果、二つの数値だけが来たものを取り出す。

part2 fn = do
  ls <- lines <$> readFile fn
  let ans = solve2 ls
  print ans

solve2 ls = sum [r1 * r2 | [r1,r2] <- elems ra]
  where
    h = length ls
    w = length $ head ls
    ca = accumArray (flip const) '.' ((0,0),(succ h, succ w))
         [ ((i,j), c) | (i,l) <- zip [1..] ls, (j,c) <- zip [1..] l]
    ra = accumArray (flip (:)) [] ((1,1),(h,w))
         [ (g, num)
         | (i,j) <- range((1,1),(h,w))
         , not (isDigit (ca ! (i,pred j))), isDigit (ca ! (i,j))
         , let k = last $ takeWhile (\j -> isDigit $ ca ! (i,j)) [j..]
         , let gs = filter (('*' ==) . (ca !)) $ (i, pred j) : (i, succ k) : [(i1, j1) | i1 <- [pred i, succ i], j1 <- [pred j .. succ k]]
         , not $ null gs
         , let num = read [ca ! (i, j1) | j1 <- [j .. k]] :: Int
         , g <- gs
         ]

{-
ghci> part2 "sample.txt"
467835
ghci> part2 "input.txt"
84399773
-}
