-- 2025/1/14 先祖返り？ spoiler用に書き直し

import Data.Array

runner i f = readFile i >>= print . f . lines

keypad = listArray ((0,0),(4,4)) $ concat ["#####", "#123#", "#456#", "#789#", "#####"]

move (x,y) 'U' = (pred x, y)
move (x,y) 'D' = (succ x, y)
move (x,y) 'L' = (x, pred y)
move (x,y) 'R' = (x, succ y)

walk keypad xy0 seq = foldl step xy0 seq
  where
    step xy d
      | keypad ! xy1 == '#' = xy
      | otherwise           = xy1
      where
        xy1 = move xy d

part1 ls = map (keypad !) $ tail $ scanl (walk keypad) (2,2) ls

test1 = runner "sample.txt" part1
main1 = runner "input.txt" part1

keypad2 = listArray ((0,0),(6,6)) $ concat
          ["...#...","..#1#..",".#234#.","#56789#",".#ABC#.","..#D#..","...#..."]

part2 ls = map (keypad2 !) $ tail $ scanl (walk keypad2) (3,1) ls

test2 = runner "sample.txt" part2
main2 = runner "input.txt" part2
