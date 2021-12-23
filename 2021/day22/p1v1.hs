import Data.List.Split
import Data.Array

compute1 xs = length $ filter id $ elems arr
  where
    ls = map parseline $ lines xs
    arr = accumArray (flip const) False ((-50,-50,-50),(50,50,50))
          [((x,y,z),b) | (b,xl,xu,yl,yu,zl,zu) <- ls, x <- [xl..xu], y <- [yl..yu], z <- [zl..zu]]

parseline l = (onoff == "on", rl xl, ru xu, rl yl, ru yu, rl zl, ru zu)
  where
    onoff:xyz:_ = words l
    [[xl,xu],[yl,yu],[zl,zu]] = map (splitOn ".." . drop 2) $ wordsBy (',' ==) xyz
    rl = max (-50) . read
    ru = min 50 . read

test1 = readFile "sample1.txt" >>= print . compute1
test2 = readFile "sample2.txt" >>= print . compute1
run1 = readFile "input.txt" >>= print . compute1

{-
*Main> test1
39
*Main> test2
590784
*Main> run1
647076
-}
