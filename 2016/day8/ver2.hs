-- 2025/1/15 for spoiler

{-
命令型な内容なので、素直にそうする。
-}

import Data.Array.IO
--import System.IO
import Control.Monad

import Data.Char
import Data.List.Split

part1 = do
  bm <- newArray ((0,0),(5,49)) '.' :: IO (IOUArray (Int,Int) Char)
  ls <- lines <$> readFile "input.txt"
  forM_ ls (\l -> do
    let [n1,n2] = map read $ wordsBy (not . isDigit) l
    case l !! 7 of
      'r' -> rot bm (mod n2 50) $ range ((n1,0),(n1,49))
      'c' -> rot bm (mod n2 6) $ range ((0,n1),(5,n1))
      _   -> rect bm n1 n2
    )
  display bm
  ans1 <- length . filter ('#' ==) <$> getElems bm
  print ans1
--  return ()

rect bm x y = forM_ (range ((0,0),(pred y, pred x))) (\p -> writeArray bm p '#')

rot :: IOUArray (Int,Int) Char -> Int -> [(Int,Int)] -> IO ()
rot bm k ps = do
  let psr = reverse ps
  bs <- forM psr (readArray bm)
  zipWithM_ (writeArray bm) psr $ drop k $ cycle bs

display bm = getElems bm >>= putStr . unlines . chunksOf 50

{-
ghci> part1
.##..####.#....####.#.....##..#...#####..##...###.
#..#.#....#....#....#....#..#.#...##....#..#.#....
#....###..#....###..#....#..#..#.#.###..#....#....
#....#....#....#....#....#..#...#..#....#.....##..
#..#.#....#....#....#....#..#...#..#....#..#....#.
.##..#....####.####.####..##....#..#.....##..###..
106
-}
