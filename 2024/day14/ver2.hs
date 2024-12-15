import Data.Char
import Data.List.Split

import Data.Array
import Data.List

import Debug.Trace

runner i f = readFile i >>= print . f. map (map read . wordsBy (not . dm)) . lines

dm '-' = True
dm c = isDigit c

test1 = runner "sample.txt" (part1 11 7)

main1 = runner "input.txt" (part1 101 103)

roboPos :: Int -> Int -> Int -> [[Int]] -> [(Int, Int)]
roboPos wide tall t nss =
  [ (mod (x + t * vx) wide, mod (y + t * vy) tall)
  | [x,y,vx,vy] <- nss
  ]

part1 :: Int -> Int -> [[Int]] -> Int
part1 wide tall nss = product $ map (cnt !) [(LT,LT),(LT,GT),(GT,LT),(GT,GT)]
  where
    cnt = accumArray (+) 0 ((LT,LT),(GT,GT))
          [ ((compare x100 ox, compare y100 oy),1)
          | (x100, y100) <- roboPos wide tall 100 nss]
    ox = div wide 2
    oy = div tall 2

distrib wide tall nss t = dist
  where
    n = length nss
    xys = roboPos wide tall t nss
    xave = div (sum $ map fst xys) n
    yave = div (sum $ map snd xys) n
    dist = div (sum [(x - xave)^2 + (y - yave)^2 | (x,y) <- xys]) n

part2D :: Int -> Int -> [[Int]] -> [(Int,Int)]
part2D wide tall nss = take 10 dts
  where
    dts = sort [(distrib wide tall nss t, t) | t <- [0 .. lcm wide tall]]

main2D = runner "input.txt" (part2D 101 103)

main2I t = readFile "input.txt" >>= putStrLn . part2I 101 103 t . map (map read . wordsBy (not . dm)) . lines

part2I :: Int -> Int -> Int -> [[Int]] -> String
part2I wide tall t nss =
    intercalate "\n" $ transpose $ chunksOf tall $ elems arr
  where
    arr = accumArray (flip const) '.' ((0,0),(wide-1, tall-1))
          [(xy, '#') | xy <- roboPos wide tall t nss]
