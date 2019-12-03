{-
それぞれの通過する格子点の集合を作っておいて、共通部分を求め、最小のものを選択する。
Setでやらないと重いかしら。
-}

import qualified Data.Set as S

main = do
    cont <- readFile "input.txt"
    let [as,bs] = map parse $ lines cont
    let ans = compute as bs
    print ans

data Dir = U | R | D | L

parse :: String -> [(Dir,Int)]
parse xs = map f ks
  where
    ks = words $ map c2s xs
    f (d:ds) = (c2d d, read ds)

-- comma 2 space
c2s ',' = ' '
c2s c = c

-- char 2 Dir
c2d 'U' = U
c2d 'R' = R
c2d 'D' = D
c2d 'L' = L

-- dir 2 delta
d2d U = (0,-1)
d2d R = (1,0)
d2d D = (0,1)
d2d L = (-1,0)

compute :: [(Dir,Int)] -> [(Dir,Int)] -> Int
compute as bs = minimum $ map md $ S.elems $ S.intersection aset bset
  where
    aset = dis2xys as
    bset = dis2xys bs

-- [(Dir,Int)] to Set (Int,Int)
dis2xys = S.fromList . tail . scanl plus (0,0) . concatMap expand
dis2xyl = tail . scanl plus (0,0) . concatMap expand

plus (x,y) (dx,dy) = (x+dx, y+dy)
expand (d,i) = replicate i (d2d d)
md (x,y) = abs x + abs y

{-
*Main> main
870
-}

{-
とりあえず、全ての交点を出して、それぞれのワイヤのそこまでの距離を前から数えることで算出して、
その最小値を選んだら行けそうだ。
と思ったらdis2xysがListでなくてSet返してた。
-}

main2 = do
    cont <- readFile "input.txt"
    let [as,bs] = map parse $ lines cont
    let ans = compute2 as bs
    print ans

compute2 :: [(Dir,Int)] -> [(Dir,Int)] -> Int
compute2 as bs = minimum [ da + db | c <- cs, let da = dist c aps, let db = dist c bps ]
  where
    aps = dis2xyl as
    bps = dis2xyl bs
    cs = S.elems $ S.intersection (S.fromList aps) (S.fromList bps)
    dist c ps = succ $ length $ takeWhile (c /=) ps

{-
*Main> main2
13698
-}