import Data.Array

import Debug.Trace

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

rot :: (Int,Int) -> (Int,Int)
rot (dx,dy) = (dy, - dx)

add :: (Int,Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c,b+d)

part12 ls = part2ans `seq` (part1ans, part2ans)
  where
-- パート1
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
-- 踏める場所はTrue
    arr0 = listArray bnds [c /= '#' | c <- concat ls]
-- スタート位置と方向
    pos0 = head [(i,j) | (i,cs) <- zip [1 ..] ls, (j,'^') <- zip [1..] cs] -- '^' 固定
    dir0 = (-1,0)
-- まっさらのマス目（踏んだ場所チェック用）
    grid0 = listArray bnds $ repeat False
-- 枠外に落ちるまで歩き続けた結果の足跡
    gridZ = loop grid0 pos0 dir0
    loop grid pos@(i,j) dir@(di,dj)
      | not $ inRange bnds pos1 = grid1
      | arr0 ! pos1 = loop grid1 pos1 dir
      | otherwise  = loop grid1 pos $ rot dir
      where
        pos1  = add pos dir
        grid1 = if grid ! pos then grid else grid // [(pos,True)]
    part1ans = length $ filter id $ elems gridZ
-- パート2
-- 指定の歩数まで歩いても落ちないならループと判定
    checkAt pos
--      | pos == pos0 = False -- ここは不可
      | not $ gridZ ! pos = False -- 通らない位置は無意味
      | otherwise = loop cnt0 pos0 dir0
      where
        cnt0 = 4 * h * w
        arr = arr0 // [(pos, False)]
        loop cnt pos@(i,j) dir@(di,dj)
          | cnt == 0 = {- traceShow "timeup" -} True
--          | cnt < cnt0, pos == pos0, dir == dir0 = traceShow "start" True -- ほぼ不在。
          | not $ inRange bnds pos1 = False
          | arr ! pos1 = loop (pred cnt) pos1 dir
          | otherwise  = loop (pred cnt) pos $ rot dir
          where
            pos1 = add pos dir
-- 最終結果、ghciだと時間かかるので途中経過を表示しながら
--    part2ans0 = length $ filter checkAt $ range bnds
    part2ans = length [() | i <- [1 .. h], traceShow i True, j <- [1 .. w], checkAt (i,j)]

-- これでもダメか。
ts (i,0) = traceShow i True
ts _ = True

test12 = runner "sample.txt" part12

main12 = runner "input.txt" part12

{-
ghci> test12
1
2
...
9
10
(41,6)
ghci> main12
1
2
...
129
130
(4656,1575)
ghci>
-}

{-
パート2について
訪問済みのマスを再訪した、というだけではダメで、向きも合っている必要がある。
そんな配列を書き換えているより、全てのマス目を全ての方向で踏めるステップ数歩いても
まだ落ちてないならループしている、でやってみたが、時間かかる。
-}
