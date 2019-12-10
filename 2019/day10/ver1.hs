import qualified Data.Set as S
import Data.List
import Data.Function

main = do
    co <- readFile "input.txt"
    let ans = compute co
    print ans

compute :: String -> (Int,(Int,Int))
compute co = maximum [ (countOne xys xy,xy) | xy <- xys ]
  where
    css = lines co
    xys = [ (x,y) | (y,cs) <- zip [0..] css, (x,c) <- zip [0..] cs, '#' == c ]

{-
ある星との相対座標がdx,dyであるとき、gcdで通分してdx1,dy1を求めて、
その組み合わせは最も手前の一つしか星は見えないので、それを正規化して数えればよい。
-}

countOne :: [(Int,Int)] -> (Int,Int) -> Int
countOne xys (x,y) = S.size $ S.fromList
  [ (dx `div` d, dy `div` d)
  | (x1,y1) <- xys
  , let dx = x1 - x, let dy = y1 - y
  , dx /= 0 || dy /= 0
  , let d = gcd (abs dx) (abs dy)]

test0 = ".#..#\n.....\n#####\n....#\n...##\n"
test1 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n"
test4 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"

{-
*Main> main
(221,(11,11))
-}

{-
この(11,11)から眺めて、それぞれの星の角度と奥行を求めて、200番目を見つける。奥行きはgcdで代用できる。
(dx1,dy1)と(dx2,dy2)を回転角で比較する関数をごりごり書くより、atan2使ってしまう方が楽な感じだ…
gcdで正規化してからatan2するからgroupも大丈夫。
-}

main2 = do
    co <- readFile "input.txt"
    let ans = compute2 co (11,11) !! 199
    print ans

compute2 :: String -> (Int,Int) -> [(Int,Int)]
compute2 co (x0, y0) = map snd rds1
  where
    css = lines co
    xys = [ (x,y) | (y,cs) <- zip [0..] css, (x,c) <- zip [0..] cs, '#' == c ]
    rds =
      [ ((angle (-dy `div` d) (dx `div` d), d), (x,y))
      | (x,y) <- xys, let dx = x - x0, let dy = y - y0, dx /= 0 || dy /= 0
      , let d = gcd (abs dx) (abs dy)
      ]
    rds1 = concat $ transpose $ groupBy ((==) `on` (fst.fst)) $ sort rds

angle x y = let t0 = atan2 (fromIntegral y) (fromIntegral x) in if t0 < 0 then t0 + 2*pi else t0

dotest2_4 = let ps = compute2 test4 (11,13) in map (ps !!) [0,1,2,9,19,49,99,198,199,200,298]
{-
*Main> dotest2_4
[(11,12),(12,1),(12,2),(12,8),(16,0),(16,9),(10,16),(9,6),(8,2),(10,9),(11,1)]
*Main> main2
(8,6)
-}