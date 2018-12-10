import Data.Array

{-

まずはファイルを読んで座標の追跡を実行するところは作れる。
全部表示するときりがなさそうなので、
座標のそれぞれの幅での最小値とその時刻を算出してみよう。

ファイルの桁が揃っているので、固定位置で読んでしまえ。

-}

main = do
  fi <- readFile "input.txt"
  let (ps,vs) = unzip $ map readem $ lines fi
  let img = compute ps vs
  mapM print img

-- 最小サイズの分析
--  let xdmin = compute ps vs
--  print xdmin

readem :: String -> ((Int,Int),(Int,Int))
readem cs = ((read x, read y),(read p, read q)) where
  (x,cs1) = splitAt 6 (drop 10 cs)
  (y,cs2) = splitAt 6 (drop 2 cs1)
  (p,cs3) = splitAt 2 (drop 12 cs2)
  q = take 2 (drop 2 cs3)

stepStar :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
stepStar ps qs = zipWith add ps qs where
  add (x,y) (p,q) = (x+p, y+q)

{-
最小サイズの分析

compute ps vs = minimum $ flip zip [10000..] $ take 10000 $ map getXd $ drop 10000 $ iterate (stepStar vs) ps
  where
    getXd ps = x2 - x1 where
--      xs = map fst ps
      xs = map snd ps
      x1 = minimum xs
      x2 = maximum xs

-}

{-
X方向は
*Main> main
(61,10619)
と判明
Y方向も
*Main> main
(9,10619)
と判明。これだけ作ればいい。
-}

-- 画像出力
compute ps vs = [ [ arr ! (x,y) | x <- [0..dx] ] | y <- [0..dy] ] where
   ps1 = (!! 10619) $ iterate (stepStar vs) ps
   x0 = minimum (map fst ps1)
   x1 = maximum (map fst ps1)
   dx = x1-x0+1
   y0 = minimum (map snd ps1)
   y1 = maximum (map snd ps1)
   dy = y1-y0+1
   arr = accumArray ovw '.' ((0,0),(dx,dy)) [((x-x0,y-y0),'#')| (x,y) <- ps1]
--   ovw _ c = c
   ovw = flip const

{-
*Main> main
"..##....#####....####...#....#.....###..#####...#....#..######."
".#..#...#....#..#....#..#....#......#...#....#..#....#..#......"
"#....#..#....#..#........#..#.......#...#....#...#..#...#......"
"#....#..#....#..#........#..#.......#...#....#...#..#...#......"
"#....#..#####...#.........##........#...#####.....##....#####.."
"######..#....#..#..###....##........#...#....#....##....#......"
"#....#..#....#..#....#...#..#.......#...#....#...#..#...#......"
"#....#..#....#..#....#...#..#...#...#...#....#...#..#...#......"
"#....#..#....#..#...##..#....#..#...#...#....#..#....#..#......"
"#....#..#####....###.#..#....#...###....#####...#....#..#......"
"..............................................................."
[(),(),(),(),(),(),(),(),(),(),()]
ABGXJBXF
-}
