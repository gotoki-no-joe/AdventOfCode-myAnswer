import Data.Array

{- 面倒なので配列でやりましょう。
activeもinactiveも見る必要があるし。
初期入力から±1大きい配列を作って、ステップごとに同様に広くなっていく感じで。
つまり最外周には#はないことが保証されているようにする。
-}

type Pocket = Array (Int,Int,Int) Bool

sample = ".#.\n..#\n###"

mydata = ".#.#.#..\n..#....#\n#####..#\n#####..#\n#####..#\n###..#.#\n#..##.##\n#.#.####"

parse :: String -> Pocket
parse txt = accumArray (||) False ((-1,-1,-1),ub) bl
  where
    ls = lines txt
    ub = (2 + length (head ls), 2 + length ls, 3)
    bl = [ ((x,y,1),True) | (y,l) <- zip [1..] ls, (x,'#') <- zip [1..] l]

step :: Pocket -> Pocket
step arr = accumArray (||) False bounds1 bl
  where
    ((x0,y0,z0),(x1,y1,z1)) = bounds arr
    bounds1 = ((pred x0, pred y0, pred z0),(succ x1, succ y1, succ z1))
    bl = [ (p,True)
         | z <- [succ z0..pred z1], y <- [succ y0..pred y1], x <- [succ x0..pred x1]
         , let p = (x,y,z), let c = count x y z
--         , c == 3 || (arr ! p && c == 4)
         , if arr ! p then c == 3 || c == 4 else c == 3
         ]
    count x y z = length [() | z <- [pred z..succ z], y <- [pred y..succ y], x <- [pred x..succ x], arr ! (x,y,z)]

-- Tのとき2or3, Fのとき3
-- その点も含めて数えると、
-- Tのとき3or4, Fのとき3
-- 言い換えると3もしくはTかつ4

countT :: Pocket -> Int
countT arr = length $ filter id $ elems arr

comp1 = countT . (!! 6) . iterate step . parse
test1 = comp1 sample
ans1 = comp1 mydata

printMap arr = unlines [ unlines [ [ if arr ! (x,y,z) then '#' else '.' | x <- [succ x0..pred x1] ] | y <- [succ y0..pred y1] ] | z <- [succ z0..pred z1] ]
  where
    ((x0,y0,z0),(x1,y1,z1)) = bounds arr

{-
*Main> test1
112
*Main> ans1
375
-}
