{-# Language Strict #-}
import Data.Array
import Control.Monad

main = do
  fi <- readFile "input.txt"
  let ls = map parse $ lines fi
  let pxs = exec ls
  let ans1 = sum $ elems pxs
  showDisp pxs
  print ans1

data Cmd = Rct Int Int | Row Int Int | Col Int Int

parse :: String -> Cmd
parse cs = case cs !! 7 of
  'r' -> makeRow cs
  'c' -> makeCol cs
  _   -> makeRct cs

makeRct cs = Rct (read as) (read bs) where
  (as,'x':bs) = break ('x' ==) $ drop 5 cs

makeRow cs = Row (read as) (read $ drop 4 bs) where
  (as, bs) = break (' ' ==) $ drop 13 cs

makeCol cs = Col (read as) (read $ drop 4 bs) where
  (as, bs) = break (' ' ==) $ drop 16 cs

bnds = ((0,0),(49,5))
xr = [0..49]
yr = [0..5]

exec ls = foldl step initial ls where
  initial = (accumArray (+) 0 bnds []) :: Array (Int,Int) Int
  step arr (Rct w h) = array bnds
    [ ((x,y), if x < w && y < h then 1 else arr ! (x,y)) | x <- xr, y <- yr ]
  step arr (Col a d) = array bnds
    [ ((x,y), arr ! (x, if x == a then (y-d+6) `mod` 6 else y)) | x <- xr, y <- yr ]
  step arr (Row b d) = array bnds
    [ ((x,y), arr ! (if y == b then (x-d+50) `mod` 50 else x,y)) | x <- xr, y <- yr ]

showDisp pxs =
  forM_ yr (\y -> do
    forM_ xr (\x -> putChar (if pxs ! (x,y) == 1 then '#' else '.'))
    putStrLn ""
  )

{-
*Main> main
.##..####.#....####.#.....##..#...#####..##...###.
#..#.#....#....#....#....#..#.#...##....#..#.#....
#....###..#....###..#....#..#..#.#.###..#....#....
#....#....#....#....#....#..#...#..#....#.....##..
#..#.#....#....#....#....#..#...#..#....#..#....#.
.##..#....####.####.####..##....#..#.....##..###..
106
-}
