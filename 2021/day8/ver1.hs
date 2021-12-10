{-
1 : **c**f* ON2なのはこれだけ
7 : a*c**f* ON3なのはこれだけ
4 : *bcd*f* ON4なのはこれだけ
8 : abcdefg ON7なのはこれだけ

6つのやつ - cdeが違う
0 : abc*efg ON6かつd=offとしたい
6 : ab*defg ON6かつc=off
9 : abcd*fg ON6かつe=OFFとしたい

5つのやつ - **b cc* e** *ff
2 : a*cde*g ON5かつf=off
3 : a*cd*fg ON5かつf=ON,C=ON
5 : ab*d*fg ON5かつc=off

a : 8, b :*6, c : 8, d : 7, e :*4, f : 9, g : 7 出現回数

+b****
ON2 & ON3 & ON4 = cf
ON61 & ON62 & ON63 = abfg
abfg & cf = f
cf - f = c
eは、ON5の中で2でのみONのもの、なのだが、どうやって導く？
ON51 & ON52 & ON53 = adg
1 - abfg = cde
cde & adg = d
cde - d - c = e
-}

-- import Data.Array
import Data.List
import Data.Function

sample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

compute11 xs = map (decode segC segE segF) rights
  where
    (lefts, _ : rights) = splitAt 10 $ words xs
    ccs = map (\cs -> (head cs, length cs)) $ group $ sort $ filter (' ' /=) $ take 58 xs
    segE = fst $ head $ filter ((4 ==) . snd) ccs
    [on2,on3,on4,on51,on52,on53,on61,on62,on63,on7]= map snd $ sort [(length l, l) | l <- lefts]
    cf = foldl1 intersect [on2, on3, on4]
    abfg = foldl1 intersect [on61, on62, on63]
    segF = head $ abfg `intersect` cf
    segC = head $ filter (segF /=) on2

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0

decode segC segE segF xs
  | l == 2 = 1
  | l == 3 = 7
  | l == 4 = 4
  | l == 7 = 8
  | l == 5 = if notElem segF xs then 2 else if notElem segC xs then 5 else 3
  | l == 6 = if notElem segE xs then 9 else if notElem segC xs then 6 else 0
  | otherwise = error "never happens please"
  where
    l = length xs

test1 = compute11 sample

run1 = do
  co <- readFile "input.txt"
  let ds = concatMap compute11 $ lines co
  print $ length $ filter (\d -> d == 1 || d == 4 || d == 7 || d == 8) ds
  return ()

main2 fn = do
  co <- readFile fn
  let ans = sum $ map (decimal . compute11) $ lines co
  print ans
  return ()

test2 = main2 "sample.txt"

run2 = main2 "input.txt"

{-
*Main> test1
[5,3,5,3]
*Main> run1
274
*Main> test2
61229
*Main> run2
1012089
-}
