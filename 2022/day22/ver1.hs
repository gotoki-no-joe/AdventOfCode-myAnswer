import Data.Array
import Data.Char

import Debug.Trace

{-
右端いっぱいまで空白で埋めてないとか何これ！死ねばいいのに！
その状態でsampleだと正解するの何これ。
-}

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls - 2
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat [take w $ l ++ repeat ' ' | l <- take h ls]
  print $ f arr (last ls)

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 fld cmds = iZ * 1000 + jZ * 4 + dZ
  where
    bnds0@(_,(h,w)) = bounds fld
    readfld ij = if inRange bnds0 ij then fld ! ij else ' '
    bnds = (((1,1),0),((h,w),3))
    g = listArray bnds $ [if c == '.' then rdlu ij d else (-1,-1) | (ij,c) <- assocs fld, d <- [0 .. 3] ]
    rdlu p@(i,j) d =
      case (readfld (i + di, j + dj), readfld p2) of
        ('.', _) -> (i + di, j + dj)
        ('#', _) -> (i, j)
        (' ', '.') -> (i2, j2)
        (' ', '#') -> (i, j)
      where
        (di,dj)= [(0,1),(1,0),(0,-1),(-1,0)] !! d
        p2@(i2,j2) = until (\p -> readfld p /= ' ') (\(i,j) -> (i + di, j + dj)) (i - di * h, j - dj * w)
    (i0,j0) = head [p | i <- [1 ..], let p = (1,i), readfld p == '.']

--    follow ijd cs | traceShow (ijd, cs) False = error ""
    follow ijd "" = ijd
    follow (ij,d) ('R':cs) = follow (ij, mod (succ d) 4) cs
    follow (ij,d) ('L':cs) = follow (ij, mod (d + 3 ) 4) cs
    follow (ij,d) cs = follow (p1,d) bs
      where
        (as,bs) = span isDigit cs
        p1 = iterate (\ij -> g ! (ij,d)) ij !! read as

    ((iZ,jZ),dZ) = follow ((i0,j0),0) cmds

{-
向きは >v<^ を 0,1,2,3 と符号化する。
'.' である全ての位置に関して、その0～3向きの前がどこかを求めてしまおう。
その後、指示書の通りにこのグラフ上を動くだけ。
-}

{-
地図の右端が ' ' 文字で埋めてあると思い込んで酷い目にあった。

ghci> test1
6032
ghci> main1
133174

パート2

どうせdirは変化しないとパート1でタカをくくっていたら、そこをいじくるのな。
というか、展開図の形が違うから、一辺の長さを与えるだけではなくて、どう折りたたむのかも
ハードコードしないと辛くないかこれ。

-}

part2sample fld cmds = 0 -- iZ * 1000 + jZ * 4 + dZ
  where
    bnds0@(_,(h,w)) = bounds fld
    readfld ij = if inRange bnds0 ij then fld ! ij else ' '
    bnds = ((1,1,0),(h,w,3))
    g = listArray bnds $ map gf $ range bnds
    gf (i,j,d)
      | c /= '.' = (-1,-1,-1) -- elsewhere
      | inRange bnds0 ij1, fld ! ij1 == '.' = (i + di, j + dj, d) -- 進める
      | inRange bnds0 ij1, fld ! ij1 == '#' = (i,j,d) -- 壁に当たる
      | i + di == 0 = 
      where
        c = fld ! (i,j)
        (di,dj)= [(0,1),(1,0),(0,-1),(-1,0)] !! d
        ij1 = (i + di, j + dj)
-- めどい。
