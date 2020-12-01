import Data.Bits (popCount)
import qualified Data.Set as S

-- 壁判定

-- x^2 + 2xy + y^2 = (x+y)^2
-- 3x + y = 2x + (x+y)
-- (x+y)^2 + (x+y) + 2x = (x+y)(x+y+1) + 2x
isOpen :: Int -> (Int, Int) -> Bool
isOpen base (x,y) = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + base

test1 = putStrLn $ unlines [ [ if isOpen 10 (x,y) then '.' else '#' | x <- [0..9] ] | y <- [0..6] ]

{-
探索
座標対の集合で幅優先探索をするだけか。
変数は
- ステップ数
- 探索済み座標set
- 新規座標set
-}

part1c :: Int -> (Int,Int) -> Int
part1c base goal = loop 0 S.empty $ S.singleton (1,1)
  where
    loop cnt visited xys
      | S.member goal xys = cnt
      | True = loop (succ cnt) v2 xys2
      where
        v2 = S.union visited xys
        xys2 = S.fromList [p | p <- udlr xys, S.notMember p v2, isOpen base p]

udlr xys =
  [ p
  | (x,y) <- S.elems xys
  , p <- [(pred x,y), (succ x,y), (x,pred y), (x, succ y)]
  , fst p >= 0, snd p >= 0
  ]

test2 = part1c 10 (7,4)

part1 = part1c 1352 (31,39)

{-
こういうときコードを使いまわせないのがもどかしい。
-}

-- part2c :: Int -> Int -> Int
part2c base cnt = loop cnt S.empty $ S.singleton (1,1)
  where
    loop 0 visited xys = S.union visited xys
    loop cnt visited xys = loop (pred cnt) v2 xys2
      where
        v2 = S.union visited xys
        xys2 = S.fromList [p | p <- udlr xys, S.notMember p v2, isOpen base p]

part2 = S.size $ part2c 1352 50

{-
*Main> test1
.#.####.##
..#..#...#
#....##...
###.#.###.
.##..#..#.
..##....#.
#...##.###

*Main> test2
11
*Main> part1
90
*Main> part2c 10 0
fromList [(1,1)]
*Main> part2c 10 1
fromList [(0,1),(1,1),(1,2)]
*Main> part2c 10 2
fromList [(0,0),(0,1),(1,1),(1,2),(2,2)]
*Main> part2
135
-}
