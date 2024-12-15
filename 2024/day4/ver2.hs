import Data.Array

runner i f = do
  ls <- lines <$> readFile i
  let h = length ls
      w = length $ head ls
      arr = listArray ((1,1),(h,w)) $ concat ls
  print $ f arr

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

part1 :: Array (Int,Int) Char -> Int
part1 arr = length
  [ ()
  | (p, 'X') <- assocs arr
  , d <- ds
  , let p1 = add p  d, readArr arr p1 == 'M'
  , let p2 = add p1 d, readArr arr p2 == 'A'
  , let p3 = add p2 d, readArr arr p3 == 'S'
  ]

readArr arr i
  | inRange (bounds arr) i = arr ! i
  | otherwise = '#'

ds :: [(Int, Int)]
ds = [(i,j) | i <- [-1 .. 1], j <- [-1 .. 1], (i,j) /= (0,0)]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a,b) (c,d) = (a+c, b+d)

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2

part2 :: Array (Int,Int) Char -> Int
part2 arr = length
  [ ()
  | (p, 'A') <- assocs arr
  , let c7 = readArr arr $ add p (-1,-1)
  , let c3 = readArr arr $ add p ( 1, 1)
  , mssm c7 c3
  , let c9 = readArr arr $ add p (-1, 1)
  , let c1 = readArr arr $ add p ( 1,-1)
  , mssm c1 c9
  ]

mssm :: Char -> Char -> Bool
mssm 'M' 'S' = True
mssm 'S' 'M' = True
mssm _ _ = False
