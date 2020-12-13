import Data.Array

sample = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

-- ひとつの場所に関して、.なら常に.に、L#なら周囲の#の数により変化がある。
one '.' _ = '.'
one 'L' 0 = '#'
one '#' n | n >= 4 = 'L'
one c _ = c

-- ある場所の周囲の'#'の数0～8を数えるには、
-- 01の列の列に直し、ずらしながら足し合わせることを繰り返せばよい。

occup '#' = 1
occup _ = 0

shiftL, shiftR :: [[Int]] -> [[Int]]
shiftL = map (\l -> tail l ++ [0])
shiftR = map (0 :) -- init

step :: Int -> Int -> [String] -> [String]
step w h ls = zipWith (zipWith one) ls sums
  where
    ds = map (map occup) ls
    zs = replicate w 0
    ds2 = tail ds ++ [zs]
    ds8 = zs : ds -- init
    ds4 = shiftR ds
    ds6 = shiftL ds
--    ds1 = shiftR ds2
--    ds3 = shiftL ds2
--    ds7 = shiftR ds8
--    ds9 = shiftL ds8
    ds1 = tail ds4 ++ [zs]
    ds3 = tail ds6 ++ [zs]
    ds7 = zs : ds4
    ds9 = zs : ds6
    sums = foldl1 (zipWith (zipWith (+))) [ds1,ds2,ds3,ds4,ds6,ds7,ds8,ds9]

comp1 str = length $ filter ('#' ==) $ concat $ fst $ head $ filter (uncurry (==)) $ zip lss (tail lss)
  where
    ls = lines str
    w = length (head ls)
    h = length ls
    lss = iterate (step w h) ls

test1 = comp1 sample

ans1 = readFile "input.txt" >>= print . comp1

{- 後半、これは面倒くさい。最初に「どこを見るか」の座標リストを作ってしまって、配列を覗くのが妥当かな。-}

ds = [(dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

type SumArray = Array (Int,Int) (Maybe [(Int,Int)])

sumarray :: Int -> Int -> [String] -> SumArray
sumarray w h ls = array ((1,1),(w,h)) [ ((i,j),f c i j) | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l ]
  where
    carr = array ((1,1),(w,h)) [ ((i,j),c) | (j,l) <- zip [1..] ls, (i,c) <- zip [1..] l ]
    f '.' _ _ = Nothing
    f 'L' i j = Just [p | (dx,dy) <- ds, Just p <- [g dx dy (i+dx) (j+dy)]]
    g dx dy x y
      | x < 1 || w < x || y < 1 || h < y = Nothing
      | carr ! (x,y) == '.' = g dx dy (x+dx) (y+dy)
      | True = Just (x,y)

step2 :: Int -> Int -> SumArray -> Array (Int,Int) Int -> Array (Int,Int) Int
step2 w h da st = array ((1,1),(w,h)) [ ((i,j), f i j) | j <- [1..h], i <- [1..w] ]
  where
    f i j = case da ! (i,j) of
      Nothing -> 0
      Just ps -> let v = sum $ map (st !) ps in
        if st ! (i,j) == 0 then if v == 0 then 1 else 0
                           else if v >= 5 then 0 else 1

comp2 str = sum $ fst $ head $ filter (uncurry (==)) $ zip sts (tail sts)
  where
    ls = lines str
    w = length (head ls)
    h = length ls
    sarr = sumarray w h ls
    iarr = accumArray (+) 0 ((1,1),(w,h)) []
    sts = map elems $ iterate (step2 w h sarr) iarr

test2 = comp2 sample

ans2 = readFile "input.txt" >>= print . comp2

{-
*Main> test1
37
*Main> ans1
2316
*Main> test2
26
*Main> ans2
2128

ちょっと時間かかるのが気になる。
-}
