import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let ans = f ls
  print ans

test1 = runner "sample.txt" part1a

main1 = runner "input.txt" part1a

test2 = runner "sample.txt" part2a

main2 = runner "input.txt" part2a

part1a ls = length $ filter id $ elems gridZ
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    arr = listArray bnds [c /= '#' | c <- concat ls]
    pos0 = head [(i,j) | (i,cs) <- zip [1 ..] ls, (j,'^') <- zip [1..] cs] -- '^' 固定
    grid0 = listArray bnds $ repeat False
    gridZ = loop grid0 pos0 (-1,0)
    loop grid pos@(i,j) dir@(di,dj)
--      | not $ inRange bnds pos = grid
      | not $ inRange bnds pos1 = grid1
      | arr ! pos1 = loop grid1 pos1 dir
      | otherwise  = loop grid1 pos $ rot dir
      where
        pos1  = add pos dir
        grid1 = if grid ! pos then grid else grid // [(pos,True)]

{-
rot (-1,0) = (0,1)
rot (0,1) = (1,0)
rot (1,0) = (0,-1)
rot (0,-1) = (-1,0)
-}
rot (dx,dy) = (dy, - dx)

add (a,b) (c,d) = (a+c,b+d)

part2a ls = length
  [ opos
  | opos <- range bnds, arr0 ! opos, opos /= pos0, loop (arr0 // [(opos,False)]) (4 * h * w) pos0 (-1,0)]
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    arr0 = listArray bnds [c /= '#' | c <- concat ls]
    pos0 = head [(i,j) | (i,cs) <- zip [1 ..] ls, (j,'^') <- zip [1..] cs] -- '^' 固定
--    gridZ = loop arr0 (h * w) pos0 (-1,0)
    loop arr cnt pos@(i,j) dir@(di,dj)
      | cnt == 0 = True
      | not $ inRange bnds pos1 = False
      | arr ! pos1 = loop arr (pred cnt) pos1 dir
      | otherwise  = loop arr (pred cnt) pos $ rot dir
      where
        pos1  = add pos dir

{-
全てのマスをチェック箇所にしたけど、part1で踏んだマスだけでいいじゃんな。
実際 130x130 = 16900 とおりのOの位置をその4倍の時間歩いて調べると
1,142,440,000
10^10になってしまった。インタプリタには荷が重い。
-}

main = test1 >> main1 >> test2 >> main2

{-
コンパイルしたら配慮無しのコードであっという間に答え出た。あらら。
> ./ver1
41
4656
6
1575
-}
