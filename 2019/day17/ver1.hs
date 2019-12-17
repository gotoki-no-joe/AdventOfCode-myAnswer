import qualified Data.IntMap as IM
import Data.Char
import Data.Array
import Data.List.Split

-- pc, base, memory, input, output
interpret :: Int -> Integer -> IM.IntMap Integer -> [Integer] -> [Integer]
interpret pc base mem js = case opcode of
    1 -> arith (+)
    2 -> arith (*)
    3 -> interpret (pc + 2) base (memwrite ad1 (head js)) (tail js)
    4 -> av1 : interpret (pc + 2) base mem js
    5 -> jump (av1 /= 0)
    6 -> jump (av1 == 0)
    7 -> set1 (av1 <  av2)
    8 -> set1 (av1 == av2)
    9 -> interpret (pc + 2) (base + av1) mem js
    99 -> []
    x -> error (unwords ["Illegal Opcode", show pc, show x])
  where
    opint = mem IM.! pc
    opcode = opint `mod` 100
    am1 = opint `div`   100 `mod` 10
    am2 = opint `div`  1000 `mod` 10
    am3 = opint `div` 10000 `mod` 10
    ma a = IM.findWithDefault 0 (fromIntegral a) mem
    ad1 = case am1 of
        0 -> ma $ pc + 1
        2 -> base + ma (pc + 1)
    ad3 = case am3 of
        0 -> ma $ pc + 3
        2 -> base + ma (pc + 3)
    av1 = case am1 of
        0 -> ma (ma $ pc + 1)
        1 -> ma (pc + 1)
        2 -> ma (base + ma (pc + 1))
    av2 = case am2 of
        0 -> ma (ma $ pc + 2)
        1 -> ma (pc + 2)
        2 -> ma (base + ma (pc + 2))
    arith op = interpret (pc + 4) base (memwrite ad3 (op av1 av2)) js
    jump True  = interpret (fromIntegral av2) base mem js
    jump False = interpret (pc + 3) base mem js
    set1 b = interpret (pc + 4) base (memwrite ad3 (if b then 1 else 0)) js
    memwrite ad v = IM.insert (fromIntegral ad) v mem

main1 = do
    li <- readFile "input.txt"
    let ia = IM.fromList $ zip [0..] $ read $ '[' : li ++ "]"
    let ans = compute1 ia
    putStrLn ans

compute1 :: IM.IntMap Integer -> String
compute1 ia = map (chr.fromIntegral) $ interpret 0 0 ia []

{-
とりあえず映像出してみる
*Main> main1
........................#############......................
........................#...........#......................
........................#...........#......................
........................#...........#......................
........................#...........#############..........
........................#.......................#..........
........................#.......................#..........
........................#.......................#..........
........................#.......................#..........
........................#.......................#..........
........................#.......................#..........
........................#.......................#..........
....................#####.......................#..........
....................#...........................#..........
....................#.........................#####.#......
....................#.........................#.#.#.#......
........#####.....#########.................#####.#.#......
........#...#.....#.#.....#.................#.#...#.#......
........#...#.....#.#.....#.................#.############^
........#...#.....#.#.....#.................#.....#.#......
#######.#...#############.#.................#.....#.#......
#.....#.#.........#.#...#.#.................#.....#.#......
#.....#.#.........#.#.#####.................#########......
#.....#.#.........#.#.#.#.........................#........
#.....#############.#####.........................#........
#.......#.............#...........................#........
#.......#.............#.......................#####........
#.......#.............#.......................#............
#########.............#.......................#............
......................#.......................#............
......................#.......................#............
......................#.......................#............
......................#.......................#............
......................#.......................#............
......................#############...........#............
..................................#...........#............
..................................#...........#............
..................................#...........#............
..................................#############............
-}

-- 手で数えるの面倒だから、この結果をArrayに入れて、上下左右全てが#な#が交差点ということで計算させる。

main2 = do
    li <- readFile "input.txt"
    let ia = IM.fromList $ zip [0..] $ read $ '[' : li ++ "]"
    let ans = compute2 ia
    print ans

sharp = 35

compute2 :: IM.IntMap Integer -> Int
compute2 ia = sum [ x * y | y <- [1..h-1], x <- [1..w-1], check x y ]
  where
    nss = linesBy (10 ==) $ interpret 0 0 ia []
    h = length nss - 1
    w = length (head nss) - 1
    a = array ((0,0),(w,h)) [ ((x,y),n) | (y,ns) <- zip [0..] nss, (x,n) <- zip [0..] ns ]
    check x y = all ((sharp ==).(a !)) [(x,y), (x-1,y), (x+1,y), (x,y-1), (x,y+1)]

{-
*Main> main2
6672
-}

-- part2

{-
これは目で見て考えていいのかな。特徴的な経路を発見する能力は人間がたけているから。

........................baaaaaaaaaaaa...................... k:8
........................b...........e...................... j:8
........................b...........e...................... i:6
........................b...........e...................... h:4
........................b...........effffffffffff.......... g:12
........................b.......................g.......... f:12
........................b.......................g.......... e:4
........................b.......................g.......... a:12
........................b.......................g.......... b:12
........................b.......................g.......... c:4
........................b.......................g.......... d:12
........................b.......................g.......... l:4
....................dcccc.......................g.......... m:4
....................d...........................g.......... n:12
....................d.........................mlgll.k......
....................d.........................m.g.d.k......
........#####.....jjjjjjjji.................hhhhg.d.k......   
........#...#.....k.d.....i.................i.m...d.k......
........#...#.....k.d.....i.................i.nnnndnknnnnn^ ^L nmldcbaefghijk A
........#...#.....k.d.....i.................i.....d.k...... B  nmldcbaefghijk GOAL
#######.#...#nnnnnkndnnnn.i.................i.....d.k......
#.....#.#...B.....k.d...m.i.................i.....d.k...... A R12R4L6L8L8L12R4R4 B
#.....#.#.........k.d.ghhhh.................ijjjjjjjj......
#.....#.#.........k.d.g.m.........................d........
#.....############k.llglm.........................d........
#.......#.........A...g...........................d........
#.......#.............g.......................ccccd........
#.......#.............g.......................b............
#########.............g.......................b............
......................g.......................b............
......................g.......................b............
......................g.......................b............
......................g.......................b............
......................g.......................b............
......................ffffffffffffe...........b............
..................................e...........b............
..................................e...........b............
..................................e...........b............
..................................aaaaaaaaaaaab............

メイン
BCCAABBCCA
共通部分まとめ
A = R------------R----L------L--------L--------
B = L------------R----R----
C = R------------R----L------------

VSCodeの選択文字列と同じ列をハイライトする機能に頼りまくった。
-}

command = map (fromIntegral.ord) $ unlines
  [ "B,C,C,A,A,B,B,C,C,A"
  , "R,12,R,4,L,6,L,8,L,8"
  , "L,12,R,4,R,4"
  , "R,12,R,4,L,12"
  , "n"
  ]--12345678901234567890 ギリギリ

main3 = do
    li <- readFile "input.txt"
    let ia = IM.fromList $ zip [0..] $ (2 :) $ tail $ read $ '[' : li ++ "]"
    let ans = compute3 ia
    print ans

compute3 :: IM.IntMap Integer -> Integer
compute3 ia = last $ interpret 0 0 ia command

{-
*Main> main3
923017
-}
