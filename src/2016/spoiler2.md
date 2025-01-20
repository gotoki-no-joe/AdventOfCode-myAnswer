# 入力

行ごとの文字列で渡す。

```
runner i f = readFile i >>= print . f . lines
```

# パート1

キーパッドの様子を二次元配列に持ち、それを指示通りに辿る。
はみ出ないように番兵で回りを囲っておく。

```haskell
import Data.Array

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
```

一行の指示に従った最終位置のボタンを押す。

```haskell
part1 ls = map (keypad !) $ tail $ scanl (walk keypad) (2,2) ls
```

# パート2

キーパッドを差し替えればよい。

```haskell
keypad2 = listArray ((0,0),(6,6)) $ concat
          ["...#...","..#1#..",".#234#.","#56789#",".#ABC#.","..#D#..","...#..."]

part2 ls = map (keypad2 !) $ tail $ scanl (walk keypad2) (3,1) ls
```
