type XYZ = (Int,Int,Int)
type System = ([XYZ],[XYZ])

zero :: XYZ
zero = (0,0,0)
vels0 = [zero,zero,zero,zero]

-- 初期位置 (input.txt)
input, sample1, sample2 :: System
input = ([(-16, 15, -9),(-14,  5,  4),(  2,  0,  6),( -3, 18,  9)],vels0)
sample1 = ([(-1, 0, 2), (2, -10, -7),(4, -8, 8),(3, 5, -1)],vels0)
sample2 = ([(-8, -10, 0),(5, 5, 10),(2, -7, 3),(9, -8, -3)],vels0)

-- 軸ごとに独立に計算できるので、そのように構成する

-- 引き寄せあいによる速度の変化は位置だけから求まり、二つの星の変化を同時に求める必要はない。
veldiff :: [Int] -> [Int]
veldiff poss = [ sum [ signum (p2-p1) | p2 <- poss ] | p1 <- poss ]

type SystemA = ([Int],[Int])

stepA :: SystemA -> SystemA
stepA (poss,vels) = (poss1, vels1)
  where
    vds = veldiff poss
    vels1 = zipWith (+) vels vds
    poss1 = zipWith (+) poss vels1

getx (x,_,_) = x
gety (_,y,_) = y
getz (_,_,z) = z

sas2s :: (SystemA,SystemA,SystemA) -> System
sas2s (xs,ys,zs) = (zip3 (fst xs) (fst ys) (fst zs), zip3 (snd xs) (snd ys) (snd zs))

s2sas :: System -> (SystemA,SystemA,SystemA)
s2sas (ps,vs) = ((map getx ps, map getx vs), (map gety ps, map gety vs), (map getz ps, map getz vs))

step :: System -> System
step s = sas2s (stepA x, stepA y, stepA z)
  where
    (x,y,z) = s2sas s

test1 = take 11 $ iterate step sample1

energy :: System -> Int
energy (poss,vels) = sum $ zipWith (*) (map f poss) (map f vels)
  where
    f (x,y,z) = abs x + abs y + abs z

test2 = energy $ iterate step sample1 !! 10

part1 = energy $ iterate step input !! 1000

{-
*Main> part1
10664
-}

-- part2

-- naiveにやってみる。

find0 :: System -> Int
find0 start = loop 1 (step start)
  where
    loop k s = if s == start then k else loop (succ k) (step s)

test3 = find0 sample1

test4 = find0 sample2

{-
*Main> test3
2772
*Main> test4
Interrupted.
もちろんウンともスンともいわない。
-}

-- 座標軸ごとに独立しているから、それぞれで求めて、最小公倍数を求める。

find1 :: System -> Int
find1 s = lcm xcount (lcm ycount zcount)
  where
    (xs0,ys0,zs0) = s2sas s
    xcount = loop xs0 1 (stepA xs0)
    ycount = loop ys0 1 (stepA ys0)
    zcount = loop zs0 1 (stepA zs0)
    loop start k s = if s == start then k else loop start (succ k) (stepA s)

test5 = find1 sample1
test6 = find1 sample2

part2 = find1 input

{-
*Main> test5
2772
*Main> test6
4686774924
*Main> part2
303459551979256
-}