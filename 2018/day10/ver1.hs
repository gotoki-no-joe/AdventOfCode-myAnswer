import Data.Array

{-
ファイル形式は桁が揃っているので決め打ちで読み込める。
position=<-31667, -52902> velocity=< 3,  5>
.........|.........|.........|.........|..
-}
read4 :: String -> ((Int,Int),(Int,Int))
read4 cs = ((get 10 6, get 18 6), (get 36 2, get 40 2))
  where
    get a b = read $ take b $ drop a cs

step1 = do
  fi <- readFile "input.txt"
  let (ps,vs) = unzip $ map read4 $ lines fi
  print $ head ps
  print $ last vs

{-
メッセージが現れる瞬間は、星がもっとも密集していると仮定する。
X,Y座標についてそれぞれ、そのような時刻と振れ幅を算出してみる。

しかし、いずれは星は離れて行ってしまうので、区間で分析する必要がある。
10000世代単位で調べる。
-}

step2 k = do
    fi <- readFile "input.txt"
    let (ps,vs) = unzip $ map read4 $ lines fi
    putStr "X axis : "
    let ans = compute k (map fst ps) (map fst vs)
    print ans
    putStr "Y axis : "
    let ans = compute k (map snd ps) (map snd vs)
    print ans

compute k ps vs = minimum $ flip zip [k10000..] $ map spectrum $ take 10000 $
                  iterate (zipWith (+) vs) $ zipWith (+) ps $ map (k10000 *) vs
  where
    k10000 = k * 10000
    spectrum xs = maximum xs - minimum xs

{-
*Main> step2 0
X axis : (6259,9999)
Y axis : (6209,9999)
*Main> step2 1
X axis : (61,10619)
Y axis : (9,10619)
*Main> step2 2
X axis : (93869,20000)
Y axis : (93819,20000)

0-9999ではまだ減少途中。2000-では既に発散。10619がビンゴ。
-}

step3 = do
    fi <- readFile "input.txt"
    let (ps,vs) = unzip $ map read4 $ lines fi
    let msg = compute2 ps vs
    putStrLn msg

compute2 :: [(Int,Int)] -> [(Int,Int)] -> String
compute2 ps vs = unlines [[arr ! (x,y) | x <- [0..dx] ] | y <- [0..dy]]
  where
    t = 10619
    f ps vs = zipWith (+) ps $ map (t *) vs
    xs = f (map fst ps) (map fst vs)
    ys = f (map snd ps) (map snd vs)
    (x0,x1,dx) = (minimum xs, maximum xs, x1 - x0)
    (y0,y1,dy) = (minimum ys, maximum ys, y1 - y0)
    arr = accumArray (flip const) '.' ((0,0),(dx,dy)) [((x - x0, y - y0), '#') | (x,y) <- zip xs ys]

{-
*Main> step3
..##....#####....####...#....#.....###..#####...#....#..######
.#..#...#....#..#....#..#....#......#...#....#..#....#..#.....
#....#..#....#..#........#..#.......#...#....#...#..#...#.....
#....#..#....#..#........#..#.......#...#....#...#..#...#.....
#....#..#####...#.........##........#...#####.....##....#####.
######..#....#..#..###....##........#...#....#....##....#.....
#....#..#....#..#....#...#..#.......#...#....#...#..#...#.....
#....#..#....#..#....#...#..#...#...#...#....#...#..#...#.....
#....#..#....#..#...##..#....#..#...#...#....#..#....#..#.....
#....#..#####....###.#..#....#...###....#####...#....#..#.....

ABGXJBXF
-}
