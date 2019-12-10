import Data.Array

main = do
    file <- readFile "input.txt"
    let claims = map parse $ lines file
    let themap = buildmap claims

    let part1 = length $ filter (1 <) $ elems themap
    print part1
    let part2 = getsafe themap claims
    print part2

{-
ファイルの内容
#1 @ 808,550: 12x22
#2 @ 486,680: 13x15
ID, x,y, w,h : IDは不要
-}

-- x1,y1,x2,y2 (w,hでなく)
type Claim = ((Int,Int),(Int,Int))

-- 行読み取り
parse :: String -> Claim
parse li = ((x, y), (x+w-1, y+h-1)) where
  l1 = drop 2 $ dropWhile ('@' /=) li
  (cx,_:l2) = break (',' ==) l1
  (cy,_:_:l3) = break (':' ==) l2
  (cw,_:ch) = break ('x' ==) l3
  [x,y,w,h] = map read [cx,cy,cw,ch]

-- 塗られた回数を配列に保持
buildmap :: [Claim] -> Array (Int,Int) Int
buildmap cs = accumArray (+) 0 ((0,0),(ux,uy)) $
  [ ((x,y),1) | ((x1,y1),(x2,y2)) <- cs, x <- [x1..x2], y <- [y1..y2]]
  where
    ux = maximum (map (fst.snd) cs)
    uy = maximum (map (snd.snd) cs)

-- 1を超えるセルがないidを探す
getsafe themap claims =
  [ id
  | (((x1,y1),(x2,y2)),id) <- zip claims [1..]
  , null [() | x <- [x1..x2], y <- [y1..y2], themap ! (x,y) > 1]
  ]

{-
*Main> main
105231
[164]
-}
