import Data.Array
import qualified Data.IntSet as IS

import Debug.Trace

part1 fn = do
  ls <- lines <$> readFile fn
  print $ solve1 ls

solve1 ls = loop 0 IS.empty initial
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    marr = listArray bnds $ concat ls
    (sx,sy) = head [(i,j) | (i,l) <- zip [1..] ls, (j,'S') <- zip [1..] l]
    initial =
      [p | 1 < sx, let p = (pred sx, sy), toSouth $ marr ! p] ++
      [p | 1 < sy, let p = (sx, pred sy), toEast  $ marr ! p] ++
      [p | sx < h, let p = (succ sx, sy), toNorth $ marr ! p] ++
      [p | sy < w, let p = (sx, succ sy), toWest  $ marr ! p] ++
      [(sx, sy)]
    loop cnt is ijs
--      | trace (show (cnt, is, ijs)) False = error "hoge"
      | null ijs = cnt -- 正解ループが先に閉じて、ダミーが途切れるのが遅かったら騙されるなぁ。
      | otherwise = loop (succ cnt) is1 ijs1
      where
        is1 = IS.union is $ IS.fromList $ map (index bnds) ijs
        ijs1 =
            [ p
            | ij@(i,j) <- ijs
            , IS.notMember (index bnds ij) is
            , let mij = marr ! ij
            , p <- [p | toNorth mij, let p = (pred i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toWest  mij, let p = (i, pred j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toSouth mij, let p = (succ i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toEast  mij, let p = (i, succ j), inRange bnds p, IS.notMember (index bnds p) is1]
            ]

toNorth c = elem c "|LJ"
toSouth c = elem c "|7F"
toEast  c = elem c "-LF"
toWest  c = elem c "-J7"

{-
普通の場所については、形から、進める先のマスがわかる。のでそこだけで生成できる。
Sについては、見えないので、その回りの内容で、Sに向かって接続しているなら接続できると仮定するしかない。

もっとくくりだししないとダメだよなぁと思いつつ、

ghci> part1 "samp1.txt"
4
ghci> part1 "samp2.txt"
8
ghci> part1 "input.txt"
7063
ghci> 140 * 140
19600

ヨシ。
-}

{-
さてパート2はどうすれば。
part1の方法でループを見つけたら、スキャンラインで、奇数回超えたマスを数えるだけか。

あかん、ヨコに伸びたパイプについて、1/2だけ数えたりしてしまう。
-}

part2 fn body = do
  ls <- lines <$> readFile fn
  print $ body ls

solve2x ls = ansX
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    marr = listArray bnds $ concat ls
    (sx,sy) = head [(i,j) | (i,l) <- zip [1..] ls, (j,'S') <- zip [1..] l]
    initial =
      [p | 1 < sx, let p = (pred sx, sy), toSouth $ marr ! p] ++
      [p | 1 < sy, let p = (sx, pred sy), toEast  $ marr ! p] ++
      [p | sx < h, let p = (succ sx, sy), toNorth $ marr ! p] ++
      [p | sy < w, let p = (sx, succ sy), toWest  $ marr ! p] ++
      [(sx, sy)]
    loop cnt is ijs
      | null ijs = is
      | otherwise = loop (succ cnt) is1 ijs1
      where
        is1 = IS.union is $ IS.fromList $ map (index bnds) ijs
        ijs1 =
            [ p
            | ij@(i,j) <- ijs
            , IS.notMember (index bnds ij) is
            , let mij = marr ! ij
            , p <- [p | toNorth mij, let p = (pred i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toWest  mij, let p = (i, pred j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toSouth mij, let p = (succ i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toEast  mij, let p = (i, succ j), inRange bnds p, IS.notMember (index bnds p) is1]
            ]
    theLoop = loop 0 IS.empty initial
    onpipe i j = IS.member (index bnds (i,j)) theLoop
    step st True = 1 - st
    step st False = st
    ansX = sum [ sum $ scanl step 0 [onpipe i j | j <- [1..w]] | i <- [1..h]]
--    ans = sum [ sum $ snd $ mapAccumL step2 0 | i <- [1..h]]

{-
なんだか面倒くさいが、それより泥臭いが、ともかく、
マスの枠線と中心線で4倍の数の格子点を考える。
パイプの上に乗っている点は、そういうものとする。
乗っていない点について、一番外と連結かどうかは、UnionFindでなくてPAINTで調べられる。

マスの中心にある点全てに関して、パイプ上でなく、PAINTで塗られていない点の個数を数える。
-}

solve2 sc ls = ansX
  where
    h = length ls
    w = length $ head ls
    bnds = ((1,1),(h,w))
    marr = listArray bnds (concat ls) // [((sx,sy), sc)]
    (sx,sy) = head [(i,j) | (i,l) <- zip [1..] ls, (j,'S') <- zip [1..] l]
    initial =
      [p | 1 < sx, let p = (pred sx, sy), toSouth $ marr ! p] ++
      [p | 1 < sy, let p = (sx, pred sy), toEast  $ marr ! p] ++
      [p | sx < h, let p = (succ sx, sy), toNorth $ marr ! p] ++
      [p | sy < w, let p = (sx, succ sy), toWest  $ marr ! p] ++
      [(sx, sy)]
    loop cnt is ijs
      | null ijs = is
      | otherwise = loop (succ cnt) is1 ijs1
      where
        is1 = IS.union is $ IS.fromList $ map (index bnds) ijs
        ijs1 =
            [ p
            | ij@(i,j) <- ijs
            , IS.notMember (index bnds ij) is
            , let mij = marr ! ij
            , p <- [p | toNorth mij, let p = (pred i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toWest  mij, let p = (i, pred j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toSouth mij, let p = (succ i, j), inRange bnds p, IS.notMember (index bnds p) is1] ++
                   [p | toEast  mij, let p = (i, succ j), inRange bnds p, IS.notMember (index bnds p) is1]
            ]
    theLoop = loop 0 IS.empty initial
-- ループを発見するここまではsolve1のまま
-- theLoop上のmarrの形状によって、PAINTの壁を立てる
    bnds2 = ((0,0), (h+h, w+w))
    bmp0 = IS.fromList
      [ index bnds2 xy
      | i <- [1..h], j <- [1..w], IS.member (index bnds (i,j)) theLoop
      , xy <- wallof i j
      ]
    wallof i j = (x, y) :
      case marr ! (i,j) of
        '-' -> [(x, pred y), (x, succ y)]
        '|' -> [(pred x, y), (succ x, y)]
        'F' -> [(succ x, y), (x, succ y)]
        'L' -> [(pred x, y), (x, succ y)]
        'J' -> [(pred x, y), (x, pred y)]
        '7' -> [(succ x, y), (x, pred y)]
      where
        x = i + i - 1
        y = j + j - 1
    bmpZ = paint bmp0 [(0,0)]
    paint bmp [] = bmp
    paint bmp (xy:xys)
      | not $ inRange bnds2 xy = paint bmp xys
      | IS.member z bmp = paint bmp xys
      | otherwise = paint (IS.insert z bmp) ((pred x, y):(succ x, y):(x, pred y):(x, succ y):xys)
      where
        z = index bnds2 xy
        (x,y) = xy
    ansX = length [() | i <- [1..h], j <- [1..w], let z = index bnds2 (i + i - 1, j + j - 1), IS.notMember z bmpZ]

{-
isのそれぞれの点について（をするのも大変な形にしてしまった）
|-FJ7L の形に基づいて、(Sについては形を引数で与えることにする) 2k+1した点を壁として登録。
さらにそこから、(0,0)からPAINTして外を塗りつぶす。
最後に、2k+1な格子点で、これに入っていない場所を数える。

ghci> part2 "samp3.txt" (solve2 'F')
4
ghci> part2 "samp4.txt" (solve2 '7')
10
ghci> part2 "input.txt" (solve2 'J')
589

はい。
-}
