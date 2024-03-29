import Data.Array
import qualified Data.Map as M

char2code '.' = 0
char2code '|' = 1
char2code '#' = 2

code2char 0 = '.'
code2char 1 = '|'
code2char 2 = '#'

step :: Array (Int,Int) Int -> Array (Int,Int) Int
step area = listArray bnds $ map f $ assocs area
  where
    bnds = bounds area
    read xy
      | inRange bnds xy = area ! xy
      | otherwise       = -1
    f ((x,y), c) =
      case c of
        0 | cnt ! 1 >= 3 -> 1
        1 | cnt ! 2 >= 3 -> 2
        2 | cnt ! 1 < 1 || cnt ! 2 < 1 -> 0
        _ -> c
      where
        cnt = mkCount [read (x1,y1) | x1 <- [pred x .. succ x], y1 <- [pred y .. succ y], x1 /= x || y1 /= y]

mkCount xs = accumArray (+) 0 (-1,2) [(x,1) | x <- xs]

showArea area = unlines $ chunksOf (y2 - y1 + 1) $ map code2char $ elems area
  where
    ((_,y1),(_,y2)) = bounds area

chunksOf _ [] = []
chunksOf n xs = let (as,bs) = splitAt n xs in as : chunksOf n bs

test = exec 10 "sample.txt"

exec sz fn = do
  arr0 <- listArray ((1,1),(sz,sz)) . map char2code . concat . lines <$> readFile fn
  let arr1 = iterate step arr0 !! 10
  let cnt = mkCount $ elems arr1
  putStrLn $ showArea arr1
  print $ cnt ! 1 * cnt ! 2

part1 = exec 50 "input.txt"

{-
> part1
...
519478
-}

{-
持続可能なら、どこかでループするはず。
合計は常にマス数なので、3つのうち2つのパラメータをキーに、時刻を記憶して、ループしていそうな箇所を探す。
エリアの内容で、本当にループしているか検証する。
-}

part2a sz fn n = do
  arr0 <- listArray ((1,1),(sz,sz)) . map char2code . concat . lines <$> readFile fn
  print $ take n $ loop 0 M.empty arr0

loop i m area
  | M.member k m = (m M.! k, i) : next
  | otherwise = next
  where
    cnt = mkCount $ elems area
    k = (cnt ! 1, cnt ! 2)
    next = loop (succ i) (M.insert k i m) (step area)

{-
ghci> part2a 10 "sample.txt" 1
[(18,19)]
ghci> part2b 10 "sample.txt" 18 19
..........
..........
..........
..........
..........
..........
..........
..........
..........
..........

..........
..........
..........
..........
..........
..........
..........
..........
..........
..........

あかんやん。
-}

part2b sz fn t1 t2 = do
  arr0 <- listArray ((1,1),(sz,sz)) . map char2code . concat . lines <$> readFile fn
  let arrs = iterate step arr0
--  putStrLn $ showArea $ arrs !! t1
--  putStrLn $ showArea $ arrs !! t2
  print (arrs !! t1 == arrs !! t2)

{-
ghci> part2a 50 "input.txt" 1
[(107,144)]
ghci> part2b 50 "input.txt" 107 144
.....................##|||||||##.........#|||.....
.....................##|||||####.........##||.....
......................########...........##|||....
.....|||..............######..............##||....
.....||||.................................##|||...
....###||.........||#.....................##||....
.....##|||.......||##...................###|||....
.....##||........||##...................|#|||.....
....##|||.......|||##...................|||||.....
..####||........|||##..............||...|||.......
####||||.......||||##............||||||...........
|#|||||........||||##..........||||||||||.........
|||||.........||||##.........|||||##|||||||.......
|||...........||||##........||||######|||||||.....
..............|||##.........||####..####|||||||...
..............|||##.........||##......####|||||||.
..............|||##...........#.........####|||||.
.............|||||##......................####||||
.............|||||##........................###|||
.............||||||##........................##|||
.............||||||##.........................##||
.............|||||||##........................##||
.............|||||||##.........................##|
.............|||||||##.........................##|
|##...........||||||##..............|||.........##
|##...........||||||##............||||||........##
|##............|||||##............||||||........##
|##............|||||##...........||||##........##|
|##.............||||##...........|||###........##|
|##.............||||##..........||||##........##||
|##..............|||##..........||||##........##||
||##.............|||##..........|||||##.....###|||
.|##..............|||##.........|||||####.####||||
.||##.............|||##.........|||||||#####||||||
..|##.........#||..|||##.........||||||||#|||||||.
..||#.........##||.|||##.........###|||||||||||...
..............##||..|||##.........####|||||||.....
..............##|||.|||##...........###||||.......
..............##||...|||##...........##||||.......
.............##|||...|||###...........##|||.......
.............##||.....|||###..........##|||.......
............##|||.....||||####.........##||.......
............##||........||||####.......##|||......
...........##|||..........||||#|........##||......
...........##||.............||||........##|||.....
.........###|||...............|.........##||......
.......####|||.........................##|||......
##...####|||||.........................##||.......
#######|||||..........................##|||.......
||###|||||............................##||........

...............................................#||
...............................................##|
......|........................................##|
.....||||.......................................##
.....#|||...........................|||........##|
....###|||..||##..................|||||||......##|
......##||..||##................|||||||||##...##||
.....##|||.||##...............|||||||||||##..###||
.....##||..||##.............|||||####||||######|||
...###|||.||##............|||||########|||###||||.
#####|||..||##...........||||####....####||||||||.
###|||||.|||##..........|||####........####|||||||
||||||...|||##..........||###............####|||||
||||....||||##.........|||##...............####|||
.|......||||##.........||##..................####|
.......|||||##.........|||##...................###
.......||||||##.........|||.....................##
.......||||||##..........|........................
.......||||||##...................................
.......||||||##......................|............
.......||||||##....................|||||..........
........|||||##..................|||||||||........
........|||||##................||||||||||||.......
.........||||##................|||||###||||.......
.........||||##...............||||######||||......
..........|||##...............|||###...##|||......
..........|||##..............||||##....#||||......
...........|||##.............|||##......|||.......
...........|||##............||||##.......|........
...........||||##...........|||##.................
...........||||##..........|||||##................
...........|||||##.........|||||##................
............||||##..........||||##................
............|||||##.........||||##................
.............||####..........|||##.............###
##............#####..........|||##...........####|
###.............####..........||...........####|||
|###.............####....................####|||||
||###...........#####.................#####|||||||
|||###..........##||##................###|||||||..
||||####.......##|||##...............##|||||||....
.|||||####...####||||##..............##|||||......
...|||||#######||||||###............##|||||.......
.....|||||###|||||||||####.........###||||........
.......|||||||||...|||||####.....####||||.........
.........|||||.......|||||####.####|||||..........
...........|...........|||||#####|||||............
.........................|||||#|||||..............
...........................|||||||................
.............................|||..................

似てるけど違うな。
arrayで保持するのは贅沢なので、50x50なら64bit word x 50 x 2 の100wordで持てるからそれで表現しようかね。

そうしないでも発見できるといいな。

ghci> part2a 50 "input.txt" 20     
[(107,144),(172,201),(236,262),(318,347),(46,364)
,(76,408),(142,430),(214,450),(316,454),(243,476)
,(375,482),(464,490),(475,503),(476,504),(477,505)
,(478,506),(479,507),(480,508),(481,509),(482,510)]
-}

part2c sz fn = do
  arr0 <- listArray ((1,1),(sz,sz)) . map char2code . concat . lines <$> readFile fn
  return $ iterate step arr0

{-
ghci> as !! 142 == as !! 430
False
ghci> as !! 214 == as !! 450
False
ghci> as !! 316 == as !! 454
False
ghci> as !! 243 == as !! 476
False
ghci> as !! 375 == as !! 482
False
ghci> as !! 464 == as !! 490
False
ghci> as !! 475 == as !! 503
True

見つけた。
53 - 475 = 28

mod (1_000_000_000 - 475) 28 = 21

ghci> mkCount $ elems $ as !! (475 + 21)
array (-1,2) [(-1,0),(0,1555),(1,584),(2,361)]
ghci> 584 * 361
210824

-}
