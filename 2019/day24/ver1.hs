import qualified Data.IntSet as IS
import Data.List

{-
隣接マスの少ないライフゲーム
-}

-- 虫がいる1いない0の長さ5x5のリストで表す。

type Field = [[Int]]

-- 隣接マスの虫の数を数えるには、1ずつずらして足し合わせる。

z5 = replicate 5 0

step :: Field -> Field
step bss = zipWith (zipWith next) bss cnt
  where
    up = z5 : bss
    dn = tail bss ++ [z5]
    rt = map (\l -> tail l ++ [0]) bss
    lt = map (0 :) bss
    cnt = add (add (add up dn) lt) rt

add = zipWith (zipWith (+))

next 1 1 = 1
next 0 1 = 1
next 0 2 = 1
next _ _ = 0

sample1 = "....#\n#..#.\n#..##\n..#..\n#....\n"
input = "##.##\n#.##.\n#...#\n##.#.\n####.\n"

readField :: String -> Field
readField = map (map (\c -> if c == '#' then 1 else 0)) . lines
showField :: Field -> String
showField = unlines . map (map (\b -> if b == 1 then '#' else '.'))

{-
*Main> sequence_ $ map (putStrLn.showField) $ take 5 $ iterate step $ readField sample1
....#
#..#.
#..##
..#..
#....

#..#.
####.
###.#
##.##
.##..

#####
....#
....#
...#.
#.###

#....
####.
...##
#.##.
.##.#

####.
....#
##..#
.....
##...
-}

-- 先に数に直して、それをIntSetに登録していって、登録済みのが出てきたら終わり。

bits = iterate (2 *) 1

f2n :: Field -> Int
f2n bss = sum $ zipWith (*) bits $ concat bss

main1 bss = loop IS.empty bss
  where
    loop m bss
      | IS.member n m = n
      | otherwise = loop (IS.insert n m) (step bss)
      where n = f2n bss

p1test = main1 (readField sample1)
part1 = main1 (readField input)

{-
*Main> p1test
2129920
*Main> part1
28778811
-}

-- part 2

{-
そうきたか！
1ステップの計算前に空のレベルを前後に追加して、また空なら削除する。

リストの前側が内側、後ろ側が外側とする。
-}

-- フィールドの任意の値を差し替える
set :: Int -> Int -> Int -> Field -> Field
set x y a bss = [ [ if x == i && y == j then a else b | (b,i) <- zip bs [0..] ] | (bs,j) <- zip bss [0..] ]

-- 内側、現在、外側を得て、現在のレベルの次の世代を求める
-- upは上隣の値。一番上A～Eのは外側8から取り出す。穴の下 18 は内側U～Yから取り出す。
-- dnは下隣の値。一番下U～Yのは外側18から取り出す。穴の上 8 は内側A～Eから取り出す。
-- ltは左隣の値。一番左AFKPUのは外側12から取り出す。穴の右 14は内側EJOTYから取り出す。
-- rtは右隣の値。一番右EJOTYのは外側14から取り出す。穴の左 12は内側AFKPUから取り出す。
newstep1 :: Field -> Field -> Field -> Field
newstep1 ass bss css = set 2 2 0 $ zipWith (zipWith next) bss cnt
  where
    up = set 2 3 (sum $ last ass) $ replicate 5 (css !! 1 !! 2) : bss
    dn = set 2 1 (sum $ head ass) $ tail bss ++ [replicate 5 (css !! 3 !! 2)]
    lt = set 3 2 (sum $ map last ass) $ let c = css !! 2 !! 1 in map (c :) bss
    rt = set 1 2 (sum $ map head ass) $ let c = css !! 2 !! 3 in map (\l -> tail l ++ [c]) bss
    cnt = add (add (add up dn) lt) rt

f0 :: Field
f0 = replicate 5 z5

newstep :: [Field] -> [Field]
newstep bsss = bsss3
  where
    bsss1 = f0 : f0 : bsss ++ [f0,f0]
    bsss2 = zipWith3 newstep1 bsss1 (tail bsss1) (drop 2 bsss1)
    bsss3 = reverse $ dropWhile (f0 ==) $ reverse $ dropWhile (f0 ==) bsss2

showFields :: [Field] -> String
showFields = unlines . map (map b2c . intercalate [2]). transpose

b2c 0 = '.'
b2c 1 = '#'
b2c 2 = '|'

{-
*Main> putStrLn $ showFields $ (!! 10) $ iterate newstep [readField sample1]
####.|.###.|..###|###..|.##..|.#...|#..##|.#.##|#.#..|...#.|..#..
#..#.|#..#.|.....|##.#.|#..##|.#.##|...##|....#|.#...|...##|.#.#.
#..#.|#....|#....|#....|....#|.#...|.....|....#|.....|.....|....#
####.|##.#.|#....|.#.##|##.##|.....|...#.|...##|.#...|...##|.#.#.
.....|.....|#...#|#.#..|#####|.....|.####|.###.|#.#..|...#.|..#..
-}

main2 bss t = sum $ concat $ concat $ (!! t) $ iterate newstep [bss]

p2test = main2 (readField sample1) 10
part2 = main2 (readField input) 200

{-
*Main> p2test
99
*Main> part2
2097
-}
